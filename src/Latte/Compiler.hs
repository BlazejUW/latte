

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Redundant bracket" #-}

module Latte.Compiler where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad (forM, forM_, unless, when, zipWithM, foldM, filterM)
import Control.Monad.State (StateT, execStateT, get, gets, modify, put)
import Data.Map (Map, adjust, delete, insert)
import qualified Data.Map as Map
import Data.Maybe (isJust, catMaybes, fromMaybe, listToMaybe)
import Data.List (intercalate)
import qualified Latte.Abs
import Latte.Helpers (Type (Boolean, Integer, String, Void), errLocation, functionName, keywordToType, name, typeToKeyword, removeLeadingPercent,
  typeToLlvmKeyword, convertToLlvmChar, convertELitIntToInt, convertListOfStmtToBlockBody, createEAdd, convertToLlvmString, isOpMul)
import Text.Read (readMaybe)

-------------------------------------------------
------------------STATE SECTION------------------
-------------------------------------------------
data CompilerState = CompilerState
  {
    compilerOutput :: CompilerOutput,
    variablesStack :: [Map String (Type,String)],
    variablesCounter :: Int,
    labelCounter :: Int,
    phiCounter :: Int,
    functionsSignatures :: Map (Latte.Abs.Ident, [Type]) Type,
    exprTypes :: Map Latte.Abs.Expr Type,
    inlineFunctions :: Map (Latte.Abs.Ident, [Type]) (Type, [Latte.Abs.Arg], Latte.Abs.Block),
    inlineFunctionsReturnStack :: [Map String String], -- (label name, register with result name)
    inlineFunctionsToken :: Int,
    inlineFunctionsLabelsStack :: [String],
    inlineReturnReached :: Bool,
    currentInlineFunctionType :: Type,
    labelsUsedInReturn :: [String],
    isBrLastStmt :: Bool,
    computedExprsStack :: [[(Latte.Abs.Expr, String)]],
    declaredVariablesInIfElseBlock :: [Map String (Type,String)],
    arguments :: Map String Type,
    stringPool :: Map String Int,
    returnReached :: Bool,
    concatWasDeclared :: Bool,
    phiNodesStack :: [Map String (Type,String)],
    labelsStack :: [String],
    inductionVariablesStack :: [Map String (Bool, Int)], -- (name: (isIV, loopConstant))
    tokenWhile :: Int,
    moreThanTwoTokensWhileIsUp :: Bool,
    tokenIfElse :: Int,
    exprsToReduceStack :: [Map Latte.Abs.Expr String], -- expr: nameToThisExpr
    variablesWithReducedExprStack :: [Map String (String,Int)], -- nameOfReducedExpr: number to add
    reducedVariablesCounter :: Int
  }
  deriving (Eq, Ord, Show)

type LCS a = StateT CompilerState (Either CompilerError) a
type CompilerError = String
type CompilerOutput = [String]

-----------------------------------------------------
------------------FUNCTIONS SECTION------------------
-----------------------------------------------------

---------------------------
--VARIABLES STACK SEGMENT--
---------------------------
getExpressionType :: Latte.Abs.Expr -> LCS Type
getExpressionType expr = do
  s <- get
  case Map.lookup expr (exprTypes s) of
    Just t -> return t
    Nothing -> do
      case expr of
        Latte.Abs.ELitInt _ _ -> do
          modify $ \s -> s { exprTypes = Map.insert expr Integer (exprTypes s) }
          return Integer
        Latte.Abs.EVar _ ident -> do
          let varName = name ident
          res <- lookupVariableWithReducedExpr varName
          case res of
            Just _ -> do
              modify $ \s -> s { exprTypes = Map.insert expr Integer (exprTypes s) }
              return Integer
            Nothing -> do
              throwError $ "Expression type not found: " ++ show expr
        Latte.Abs.EAdd _ expr1 _ expr2 -> do
          expr1Type <- getExpressionType expr1
          expr2Type <- getExpressionType expr2
          if expr1Type == Integer && expr2Type == Integer then do
            modify $ \s -> s { exprTypes = Map.insert expr Integer (exprTypes s) }
            return Integer
          else do
            throwError $ "Expression type not found: " ++ show expr
        _ -> throwError $ "Expression type not found: " ++ show expr

getVariableType :: String -> LCS Type
getVariableType identifier = do
  state <- get
  let findTypeInStack [] = error $ "Variable " ++ identifier ++ " not found"
      findTypeInStack (frame:rest) = case Map.lookup identifier frame of
        Just (varType, _) -> return varType
        Nothing -> findTypeInStack rest
  findTypeInStack (variablesStack state)

pushVariablesFrame :: LCS ()
pushVariablesFrame = modify $ \s -> s { variablesStack = Map.empty : variablesStack s }

popVariablesFrame :: LCS ()
popVariablesFrame = modify $ \s -> s {
  variablesStack = case variablesStack s of
      (_:rest) -> rest
      [] -> []
  }

addVariableToFrame :: String -> Type -> String  -> LCS ()
addVariableToFrame varName varType llvmVarName = modify $ \s ->
  let (currentFrame:rest) = variablesStack s
  in s { variablesStack = Map.insert varName (varType, llvmVarName) currentFrame : rest }

updateVariableInStack :: String -> Type -> String -> LCS ()
updateVariableInStack varName varType newReg = do
  state <- get
  let frames = variablesStack state
  let updatedFrames = updateFrames frames
  put state { variablesStack = updatedFrames }
  where
    updateFrames [] = []
    updateFrames (frame:rest) =
      if Map.member varName frame
      then updateVariableInFrame frame varName varType newReg:rest
      else frame:updateFrames rest

updateVariableInFrame :: Map String (Type, String) -> String -> Type -> String -> Map String (Type, String)
updateVariableInFrame frame varName varType newReg =
  case Map.lookup varName frame of
    Just (varType, _) ->
      let updatedFrame = Map.insert varName (varType, newReg) frame
      in updatedFrame
    Nothing -> frame

findOrDeclareString :: String -> LCS Int
findOrDeclareString str = do
  s <- get
  let stringPool = Latte.Compiler.stringPool s
  case Map.lookup str stringPool of
    Just index -> return index
    Nothing -> do
      let index = Map.size stringPool
      modify $ \s -> s { stringPool = Map.insert str index stringPool }
      return index

getVariableNameFromStack :: String -> LCS String
getVariableNameFromStack varName = do
  maybeVar <- lookupVariable varName
  case maybeVar of
    Just (_, llvmVarName) -> return llvmVarName
    Nothing -> do
      s <- get
      throwError $ "Variable not defined in stack: " ++ varName

lookupVariable :: String -> LCS (Maybe (Type, String))
lookupVariable varName = gets $ \s ->
  let search [] = Nothing
      search (frame:rest) = case Map.lookup varName frame of
        Nothing -> search rest
        justVar -> justVar
  in search (variablesStack s)

getNextVariableAndUpdate :: LCS String
getNextVariableAndUpdate = do
  s <- get
  let counter = variablesCounter s
  modify $ \s -> s { variablesCounter = counter + 1 }
  return ("." ++ show counter)

pushDeclaredVariablesFrame :: LCS ()
pushDeclaredVariablesFrame = modify $ \s -> s { declaredVariablesInIfElseBlock = Map.empty : declaredVariablesInIfElseBlock s }

popDeclaredVariablesFrame :: LCS ()
popDeclaredVariablesFrame = modify $ \s -> s {
  declaredVariablesInIfElseBlock = case declaredVariablesInIfElseBlock s of
      (_:rest) -> rest
      [] -> []
  }

getTopFrameOfDeclaredVariables :: LCS (Map String (Type, String))
getTopFrameOfDeclaredVariables = gets $ \s -> case declaredVariablesInIfElseBlock s of
  (topFrame:_) -> topFrame
  _ -> Map.empty

potentiallyAddDeclaredVariableToFrame :: String -> Type -> String  -> LCS ()
potentiallyAddDeclaredVariableToFrame varName varType llvmVarName = do
  s <- get
  let variableExistsInStack = any (Map.member varName) (variablesStack s)
  when variableExistsInStack $ do
    let (currentFrame:rest) = declaredVariablesInIfElseBlock s
    modify $ \s -> s { declaredVariablesInIfElseBlock = Map.insert varName (varType, llvmVarName) currentFrame : rest }

-----------------------------
--EXPRESSIONS STACK SEGMENT--
-----------------------------
pushExprsFrame :: LCS ()
pushExprsFrame = modify $ \s -> s { computedExprsStack = [] : computedExprsStack s }

popExprsFrame :: LCS ()
popExprsFrame = modify $ \s -> s {
  computedExprsStack = case computedExprsStack s of
      (_:rest) -> rest
      [] -> []
  }

addExprToFrame :: Latte.Abs.Expr -> String -> LCS ()
addExprToFrame expr llvmVarName = modify $ \s ->
  case computedExprsStack s of
    (currentFrame:rest) ->
      s { computedExprsStack = ((expr, llvmVarName) : currentFrame) : rest }
    [] ->
      s { computedExprsStack = [[(expr, llvmVarName)]] }

combineTypeAndIndentOfExpr :: Latte.Abs.Expr -> String -> LCS String
combineTypeAndIndentOfExpr expr exprStr = do
  exprType <- getExpressionType expr
  return $ typeToLlvmKeyword exprType ++ " " ++ exprStr

compareLatteAddOp :: Latte.Abs.AddOp -> Latte.Abs.AddOp -> Bool
compareLatteAddOp op1 op2 = case (op1, op2) of
    (Latte.Abs.Plus _, Latte.Abs.Plus _)   -> True
    (Latte.Abs.Minus _, Latte.Abs.Minus _) -> True
    _                  -> False

compareLatteMulOp :: Latte.Abs.MulOp -> Latte.Abs.MulOp -> Bool
compareLatteMulOp op1 op2 = case (op1, op2) of
    (Latte.Abs.Times _, Latte.Abs.Times _) -> True
    (Latte.Abs.Div _, Latte.Abs.Div _)     -> True
    (Latte.Abs.Mod _, Latte.Abs.Mod _)     -> True
    _                  -> False

compareLatteRelOp :: Latte.Abs.RelOp -> Latte.Abs.RelOp -> Bool
compareLatteRelOp op1 op2 = case (op1, op2) of
    (Latte.Abs.LTH _, Latte.Abs.LTH _) -> True
    (Latte.Abs.LE _, Latte.Abs.LE _)   -> True
    (Latte.Abs.GTH _, Latte.Abs.GTH _) -> True
    (Latte.Abs.GE _, Latte.Abs.GE _)   -> True
    (Latte.Abs.EQU _, Latte.Abs.EQU _) -> True
    (Latte.Abs.NE _, Latte.Abs.NE _)   -> True
    _                  -> False

isExprEqual :: Latte.Abs.Expr -> Latte.Abs.Expr -> LCS Bool
isExprEqual expr1 expr2 = case (expr1, expr2) of
  (Latte.Abs.ELitTrue _, Latte.Abs.ELitTrue _) -> return True
  (Latte.Abs.ELitFalse _, Latte.Abs.ELitFalse _) -> return True
  (Latte.Abs.ELitInt _ val1, Latte.Abs.ELitInt _ val2) -> return $ val1 == val2
  (Latte.Abs.EString _ str1, Latte.Abs.EString _ str2) -> return $ str1 == str2
  (Latte.Abs.EVar _ ident1, Latte.Abs.EVar _ ident2) -> return $ name ident1 == name ident2
  (Latte.Abs.EAdd _ expr11 op1 expr12, Latte.Abs.EAdd _ expr21 op2 expr22) -> do
    expr11EqExpr21 <- isExprEqual expr11 expr21
    expr12EqExpr22 <- isExprEqual expr12 expr22
    let opEq = compareLatteAddOp op1 op2
    expr11EqExpr22 <- isExprEqual expr11 expr22
    expr12EqExpr21 <- isExprEqual expr12 expr21
    return $ ((expr11EqExpr21 && expr12EqExpr22) || (expr11EqExpr22 && expr12EqExpr21 ))  && opEq
  (Latte.Abs.EMul _ expr11 op1 expr12, Latte.Abs.EMul _ expr21 op2 expr22) -> do
    expr11EqExpr21 <- isExprEqual expr11 expr21
    expr12EqExpr22 <- isExprEqual expr12 expr22
    let opEq = compareLatteMulOp op1 op2
    expr11EqExpr22 <- isExprEqual expr11 expr22
    expr12EqExpr21 <- isExprEqual expr12 expr21
    return $ (expr11EqExpr21 && expr12EqExpr22 && opEq) || (expr11EqExpr22 && expr12EqExpr21 && opEq && isOpMul op1)
  (Latte.Abs.EAnd _ expr11 expr12, Latte.Abs.EAnd _ expr21 expr22) -> do
    expr11EqExpr21 <- isExprEqual expr11 expr21
    expr12EqExpr22 <- isExprEqual expr12 expr22
    expr11EqExpr22 <- isExprEqual expr11 expr22
    expr12EqExpr21 <- isExprEqual expr12 expr21
    return $ ((expr11EqExpr21 && expr12EqExpr22) || (expr11EqExpr22 && expr12EqExpr21 ))
    -- return $ expr11EqExpr21 && expr12EqExpr22
  (Latte.Abs.EOr _ expr11 expr12, Latte.Abs.EOr _ expr21 expr22) -> do
    expr11EqExpr21 <- isExprEqual expr11 expr21
    expr12EqExpr22 <- isExprEqual expr12 expr22
    expr11EqExpr22 <- isExprEqual expr11 expr22
    expr12EqExpr21 <- isExprEqual expr12 expr21
    return $ ((expr11EqExpr21 && expr12EqExpr22) || (expr11EqExpr22 && expr12EqExpr21 ))
    -- return $ expr11EqExpr21 && expr12EqExpr22
  (Latte.Abs.Neg _ expr1, Latte.Abs.Neg _ expr2) -> isExprEqual expr1 expr2
  (Latte.Abs.Not _ expr1, Latte.Abs.Not _ expr2) -> isExprEqual expr1 expr2
  (Latte.Abs.ERel _ expr11 op1 expr12, Latte.Abs.ERel _ expr21 op2 expr22) -> do
    expr11EqExpr21 <- isExprEqual expr11 expr21
    expr12EqExpr22 <- isExprEqual expr12 expr22
    let opEq = compareLatteRelOp op1 op2
    return $ expr11EqExpr21 && expr12EqExpr22 && opEq
  _ -> return False

lookupExprsStack :: Latte.Abs.Expr -> LCS (Maybe String)
lookupExprsStack expr = do
  frames <- gets computedExprsStack
  searchExprsStack frames expr

searchExprsStack :: [[(Latte.Abs.Expr, String)]] -> Latte.Abs.Expr -> LCS (Maybe String)
searchExprsStack [] _ = return Nothing
searchExprsStack (frame:rest) exprToSearch = do
  result <- searchExprsFrame frame exprToSearch
  case result of
    Just llvmVarName -> return $ Just llvmVarName
    Nothing -> searchExprsStack rest exprToSearch

lookupExprsFrame :: Latte.Abs.Expr -> LCS (Maybe String)
lookupExprsFrame expr = do
  frames <- gets computedExprsStack
  topFrame <- case frames of
    (topFrame:_) -> do
      return topFrame
    _ -> return []
  searchExprsFrame topFrame expr

searchExprsFrame :: [(Latte.Abs.Expr, String)]-> Latte.Abs.Expr  -> LCS (Maybe String)
searchExprsFrame [] _ = return Nothing
searchExprsFrame ((expr, llvmVarName):rest) exprToSearch = do
  exprsEqual <- isExprEqual expr exprToSearch
  if exprsEqual then
    return $ Just llvmVarName
  else
    searchExprsFrame rest exprToSearch

removeExprsWithVarFromAllFrames :: String -> LCS ()
removeExprsWithVarFromAllFrames varName = do
  frames <- gets computedExprsStack
  newFrames <- mapM (removeExprsWithVarFromOneFrame varName) frames
  modify $ \s -> s { computedExprsStack = newFrames }

removeExprsWithVarFromOneFrame :: String -> [(Latte.Abs.Expr, String)] -> LCS [(Latte.Abs.Expr, String)]
removeExprsWithVarFromOneFrame varName frame = return $ filter (not . containsVar varName) frame

findCommonExprs :: [(Latte.Abs.Expr, String)] -> [(Latte.Abs.Expr, String)] -> LCS [(Latte.Abs.Expr, String)]
findCommonExprs ifFrame elseFrame = do
  filterM (isInBothFrames elseFrame) ifFrame

isInBothFrames :: [(Latte.Abs.Expr, String)] -> (Latte.Abs.Expr, String) -> LCS Bool
isInBothFrames frame (expr, _) = anyM (isExprEqual expr . fst) frame

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = foldr (\x acc -> p x >>= \res -> if res then return True else acc) (return False)

mergeTopFrameInElseIf :: LCS ()
mergeTopFrameInElseIf = do
  s <- get
  stack <- gets computedExprsStack
  case stack of
    (topFrame:nextFrame:rest) -> do
      mergedFrame <- mergeFrames topFrame nextFrame
      modify $ \s -> s { computedExprsStack = mergedFrame : rest }
    [topFrame] -> do
      modify $ \s -> s { computedExprsStack = [topFrame] }
    _ -> do
      return ()

mergeFrames :: [(Latte.Abs.Expr, String)] -> [(Latte.Abs.Expr, String)] -> LCS [(Latte.Abs.Expr, String)]
mergeFrames topFrame nextFrame = do
  filteredNextFrame <- filterM (\(expr, _) -> not <$> anyMForMerge (isExprEqual expr . fst) topFrame) nextFrame
  return $ topFrame ++ filteredNextFrame

anyMForMerge :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyMForMerge p = foldr (\x acc -> p x >>= \res -> if res then return True else acc) (return False)

getTopFrame :: [[(Latte.Abs.Expr, String)]] -> [(Latte.Abs.Expr, String)]
getTopFrame stack = case stack of
  (topFrame:_) -> topFrame
  _ -> []

updateExprsStackAfterIfElse:: String -> String -> [[(Latte.Abs.Expr, String)]] -> [[(Latte.Abs.Expr, String)]] -> LCS ([String])
updateExprsStackAfterIfElse ifLabel elseLabel stackAfterIf stackAfterElse= do
  s <- get
  (phiNodes, updatedStackAfterIf, updatedStackAfterElse) <- updateFramesAfterCondElse ifLabel elseLabel stackAfterIf stackAfterElse
  updatedStack <- updateStackWithCommonExprs updatedStackAfterIf updatedStackAfterElse
  modify $ \s -> s { computedExprsStack = updatedStack }
  return phiNodes

updateFramesAfterCondElse :: String -> String -> [[(Latte.Abs.Expr, String)]] -> [[(Latte.Abs.Expr, String)]] -> LCS ([String], [[(Latte.Abs.Expr, String)]], [[(Latte.Abs.Expr, String)]])
updateFramesAfterCondElse ifLabel elseLabel stackAfterIf stackAfterElse = do
    (phiNodes, updatedStackAfterIf, updatedStackAfterElse) <- foldM (processFrames ifLabel elseLabel) ([], [], []) (zip stackAfterIf stackAfterElse)
    return (phiNodes, reverse updatedStackAfterIf, reverse updatedStackAfterElse)

processFrames :: String -> String -> ([String], [[(Latte.Abs.Expr, String)]], [[(Latte.Abs.Expr, String)]]) -> ([(Latte.Abs.Expr, String)], [(Latte.Abs.Expr, String)]) -> LCS ([String], [[(Latte.Abs.Expr, String)]], [[(Latte.Abs.Expr, String)]])
processFrames ifLabel elseLabel (accPhi, accIf, accElse) (ifFrame, elseFrame) = do
    (phiNodes, updatedIfFrame, updatedElseFrame) <- createPhiNodesAndUpdateFramesWithExprsAfterCondElse ifLabel elseLabel ifFrame elseFrame
    return (phiNodes ++ accPhi, updatedIfFrame : accIf, updatedElseFrame : accElse)

updateStackWithCommonExprs :: [[(Latte.Abs.Expr, String)]] -> [[(Latte.Abs.Expr, String)]] -> LCS [[(Latte.Abs.Expr, String)]]
updateStackWithCommonExprs [] [] = return []
updateStackWithCommonExprs (ifFrame:ifRest) (elseFrame:elseRest) = do
  commonExprs <- findCommonExprs ifFrame elseFrame
  rest <- updateStackWithCommonExprs ifRest elseRest
  return (commonExprs : rest)
updateStackWithCommonExprs _ _ = error "Stacks do not match in size"

updateFrameWithCommonExprs :: [(Latte.Abs.Expr, String)] -> [(Latte.Abs.Expr, String)] -> [(Latte.Abs.Expr, String)]
updateFrameWithCommonExprs frame commonExprs = map (\(expr, var) -> case lookup expr commonExprs of
  Just newVar -> (expr, newVar)
  Nothing -> (expr, var)) frame

removeDeclaredVariablesFromExprsStack :: [[(Latte.Abs.Expr, String)]] -> Map String (Type, String) -> LCS [[(Latte.Abs.Expr, String)]]
removeDeclaredVariablesFromExprsStack exprsStack declaredVariablesFrame = do
  case exprsStack of
        (topFrame:rest) -> do
          updatedExprsFrame <- foldM processVariable topFrame (Map.keys declaredVariablesFrame)
          return $ updatedExprsFrame : rest
        _ -> return exprsStack

processVariable :: [(Latte.Abs.Expr, String)] -> String -> LCS [(Latte.Abs.Expr, String)]
processVariable frame varName = removeExprsWithVarFromOneFrame varName frame

containsVar :: String -> (Latte.Abs.Expr, String) -> Bool
containsVar varName (expr, _) = checkExpr expr
  where
    checkExpr :: Latte.Abs.Expr -> Bool
    checkExpr e = case e of
      Latte.Abs.EVar _ ident -> name ident == varName
      Latte.Abs.EAdd _ expr1 _ expr2 -> checkExpr expr1 || checkExpr expr2
      Latte.Abs.EMul _ expr1 _ expr2 -> checkExpr expr1 || checkExpr expr2
      Latte.Abs.EAnd _ expr1 expr2 -> checkExpr expr1 || checkExpr expr2
      Latte.Abs.EOr _ expr1 expr2 -> checkExpr expr1 || checkExpr expr2
      Latte.Abs.Neg _ expr -> checkExpr expr
      Latte.Abs.Not _ expr -> checkExpr expr
      Latte.Abs.ERel _ expr1 _ expr2 -> checkExpr expr1 || checkExpr expr2
      Latte.Abs.EApp _ _ args -> any checkExpr args
      _ -> False

------------------------
--REDUCE STACK SEGMENT--
------------------------
pushReduceStacks :: LCS ()
pushReduceStacks = do
  pushReducedExprsFrame
  pushVariablesWithReducedExprStack

popReduceStacks :: LCS ()
popReduceStacks = do
  popReducedExprsFrame
  popVariablesWithReducedExprStack

pushReducedExprsFrame :: LCS ()
pushReducedExprsFrame = modify $ \s -> s { exprsToReduceStack = Map.empty : exprsToReduceStack s }

popReducedExprsFrame :: LCS ()
popReducedExprsFrame = modify $ \s -> s {
  exprsToReduceStack = case exprsToReduceStack s of
      (_:rest) -> rest
      [] -> []
  }

pushVariablesWithReducedExprStack :: LCS ()
pushVariablesWithReducedExprStack = modify $ \s -> s { variablesWithReducedExprStack = Map.empty : variablesWithReducedExprStack s }

popVariablesWithReducedExprStack :: LCS ()
popVariablesWithReducedExprStack = modify $ \s -> s {
  variablesWithReducedExprStack = case variablesWithReducedExprStack s of
      (_:rest) -> rest
      [] -> []
  }

addVariableWithReducedExprToFrame :: String -> String ->  Int -> LCS ()
addVariableWithReducedExprToFrame varName iv val = modify $ \s ->
  let (currentFrame:rest) = variablesWithReducedExprStack s
  in s { variablesWithReducedExprStack = Map.insert varName (iv,val) currentFrame : rest }

lookupVariableWithReducedExpr :: String -> LCS (Maybe (String,Int))
lookupVariableWithReducedExpr varName = do
  frames <- gets variablesWithReducedExprStack
  topFrame <- case frames of
    (topFrame:_) -> do
      return topFrame
    _ -> return Map.empty
  return $ Map.lookup varName topFrame

createVariableWithExprToReduce :: Latte.Abs.Expr -> LCS String
createVariableWithExprToReduce expr = do
  s <- get
  let frames = exprsToReduceStack s
  let topFrame = case frames of
        (topFrame:_) -> topFrame
        _ -> Map.empty
  let maybeVar = Map.lookup expr topFrame
  case maybeVar of
    Just varName -> return varName
    Nothing -> do
      let varName = "_reducedVariable" ++ show (reducedVariablesCounter s)
      modify $ \s -> s { reducedVariablesCounter = reducedVariablesCounter s + 1 }
      modify $ \s -> s { exprsToReduceStack = Map.insert expr varName topFrame : frames }
      return varName

createDeclToReducedExprs :: LCS (Maybe Latte.Abs.Stmt)
createDeclToReducedExprs = do
    frames <- gets exprsToReduceStack
    case frames of
        (frame:_) -> do
            let items = Map.toList frame
            let declItems = map createDeclItem items
            return $ Just $ Latte.Abs.Decl Latte.Abs.BNFC'NoPosition (Latte.Abs.Int Latte.Abs.BNFC'NoPosition) declItems
        _ -> return Nothing

createDeclItem :: (Latte.Abs.Expr, String) -> Latte.Abs.Item
createDeclItem (expr, varName) = do
  Latte.Abs.Init Latte.Abs.BNFC'NoPosition (Latte.Abs.Ident varName) expr

createMapFromList :: [(String, Latte.Abs.Stmt)] -> Map String [Latte.Abs.Stmt]
createMapFromList = foldl insertStmt Map.empty
  where
    insertStmt m (k, v) = Map.insertWith (++) k [v] m

createAssignmentBlockWithReducedExpression :: LCS [(String,Latte.Abs.Stmt)]
createAssignmentBlockWithReducedExpression = do
  frames <- gets exprsToReduceStack
  case frames of
    (topFrame:_) -> do
      let items = Map.toList topFrame
      mapM createNewAssignmentForReducedVariable items
    _ -> return []

createNewAssignmentForReducedVariable :: (Latte.Abs.Expr, String) -> LCS (String, Latte.Abs.Stmt)
createNewAssignmentForReducedVariable (expr, name) = do
  result <- lookupVariableWithReducedExpr name
  let p = Latte.Abs.BNFC'NoPosition
  let ident = Latte.Abs.Ident name
  case result of
    Just (iv,number) -> do
      let num = fromIntegral number
      let add = createEAdd p ident num
      return (iv,Latte.Abs.Ass p ident add)
    Nothing -> error $ "Something went wrong in function createNewAssignmentForReducedVariable with variable" ++ name

------------------------
--LABELS STACK SEGMENT--
------------------------
getNextLabelCounterAndUpdate :: LCS Int
getNextLabelCounterAndUpdate = do
  s <- get
  let counter =  labelCounter s + 1
  modify $ \s -> s { labelCounter = counter }
  return counter

getNextPhiCounterAndUpdate :: LCS Int
getNextPhiCounterAndUpdate = do
  s <- get
  let counter =  phiCounter s + 1
  modify $ \s -> s { phiCounter = counter }
  return counter

getTopLabel :: LCS String
getTopLabel = gets $ \s -> case labelsStack s of
  (topLabel:_) -> topLabel
  _ -> error "No label on stack"

pushLabelToStack :: String -> LCS ()
pushLabelToStack label = modify $ \s -> s { labelsStack = label : labelsStack s }

popLabelFromStack :: LCS ()
popLabelFromStack = modify $ \s -> s {
  labelsStack = case labelsStack s of
      (_:rest) -> rest
      [] -> []
  }

----------------
--LOOP SEGMENT--
----------------
tokenWhileUp :: LCS Int
tokenWhileUp = do
  s <- get
  modify $ \s -> s { tokenWhile = tokenWhile s + 1 }
  return $ tokenWhile s

tokenWhileDown :: LCS Int
tokenWhileDown = do
  s <- get
  modify $ \s -> s { tokenWhile = tokenWhile s - 1 }
  return $ tokenWhile s

isAnyTokenWhileUp :: LCS Bool
isAnyTokenWhileUp = do
  s <- get
  return $ tokenWhile s > 0

isMoreThanTwoTokensWhileUp :: LCS Bool
isMoreThanTwoTokensWhileUp = do
  s <- get
  isMoreThanTwo <- gets moreThanTwoTokensWhileIsUp
  if isMoreThanTwo then do
    when (tokenWhile s == 1) $ do
      modify $ \s -> s { moreThanTwoTokensWhileIsUp = False }
    return $ isMoreThanTwo
  else do
    when (tokenWhile s > 2) $ do
      modify $ \s -> s { moreThanTwoTokensWhileIsUp = True }
    return $ tokenWhile s > 2

commonWhilePart :: Latte.Abs.BNFC'Position -> Latte.Abs.Expr -> Latte.Abs.Stmt -> String ->
          String -> String -> Int -> Bool -> StateT CompilerState (Either CompilerError) ()
commonWhilePart p expr stmt condLabel bodyLabel endLabel counter isAlwaysTrue = do
    tokenWhileUp
    varStackBefore <- gets variablesStack
    pushPhiNodesFrame
    originalReturnFlag <- gets returnReached
    modify $ \s -> s { returnReached = False }
    pushExprsFrame
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ condLabel] }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [condLabel ++ ":"] }
    pushPhiNodesFrame
    fakeWhileRun expr stmt counter
    variablesStackAfterFakeLoop <- gets variablesStack
    popPhiNodesFrameAndMergeItIntoStack
    e <- compilerExpr expr
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ e ++ ", label %" ++ bodyLabel ++ ", label %" ++ endLabel] }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [bodyLabel ++ ":"] }
    ivsToString <- formatInductionVariables
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["; Induction variables: " ++ ivsToString] }
    phiNodes <- gets phiNodesStack
    pushLabelToStack bodyLabel
    compile stmt
    popExprsFrame
    isMoreThanTwoTokensWhileUp <- isMoreThanTwoTokensWhileUp
    phiNodeTopFrameAfterEverytthing <- (if isMoreThanTwoTokensWhileUp then (do
      case phiNodes of
        (topFrame:_) -> return topFrame
        _ -> return Map.empty
      ) else (do
      popPhiNodesFrameAndMergeItIntoStack
      return Map.empty
      ))
    modify $ \s -> s { variablesStack = variablesStackAfterFakeLoop }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ condLabel] }
    modify $ \s -> s { returnReached = originalReturnFlag}
    tokenWhileDown
    if isAlwaysTrue then
      return ()
    else do
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
      when isMoreThanTwoTokensWhileUp $ do
        topLabelBeforeEnd <- getTopLabel
        phiBlock <- createSinglePhiBlock varStackBefore phiNodeTopFrameAfterEverytthing condLabel
        modify $ \s -> s { compilerOutput = compilerOutput s ++ phiBlock }
      popPhiNodesFrameAndMergeItIntoStack
      pushLabelToStack endLabel

--------------------------
--STMT FUNCTIONS SEGMENT--
--------------------------
printArg :: Latte.Abs.Arg -> String
printArg (Latte.Abs.Arg _ argType ident) =
    typeToLlvmKeyword (keywordToType argType) ++ " %" ++ name ident

getArgsTypesFromFnDef :: [Latte.Abs.Arg] -> [Type]
getArgsTypesFromFnDef = map (\(Latte.Abs.Arg _ argType _) -> keywordToType argType)

fakeInitInsteadOfAlloca :: String -> Type -> String -> Latte.Abs.Expr' a -> Bool -> StateT CompilerState (Either CompilerError) ()
fakeInitInsteadOfAlloca varName varType exprString expr isItAssignment = case expr of
  Latte.Abs.ELitInt _ _ -> do
    addCounter <- getNextVariableAndUpdate

    let addInstr = "%" ++ addCounter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, " ++ exprString
    if isItAssignment then do
      updateVariableInStack varName varType addCounter
      addPhiNodeToFrame varName varType addCounter
    else do
      inIfElse <- isAnyTokenIfElseUp
      when inIfElse $ do
        potentiallyAddDeclaredVariableToFrame varName varType addCounter
      addVariableToFrame varName varType addCounter
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
  Latte.Abs.ELitTrue _ -> do
    addCounter <- getNextVariableAndUpdate
    let addInstr = "%" ++ addCounter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, 1"
    if isItAssignment then do
      updateVariableInStack varName varType ( addCounter)
      addPhiNodeToFrame varName varType ( addCounter)
    else do
      inIfElse <- isAnyTokenIfElseUp
      when inIfElse $ do
        potentiallyAddDeclaredVariableToFrame varName varType addCounter
      addVariableToFrame varName varType ( addCounter)
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
  Latte.Abs.ELitFalse _ -> do
    addCounter <- getNextVariableAndUpdate
    let addInstr = "%" ++  addCounter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, 0"
    if isItAssignment then do
      updateVariableInStack varName varType ( addCounter)
      addPhiNodeToFrame varName varType ( addCounter)
    else do
      inIfElse <- isAnyTokenIfElseUp
      when inIfElse $ do
        potentiallyAddDeclaredVariableToFrame varName varType addCounter
      addVariableToFrame varName varType ( addCounter)
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
  Latte.Abs.EVar _ ident -> do
    itIsInWhile <- isAnyTokenWhileUp
    if itIsInWhile then do
      let varNameOfEVar = name ident
      maybeVar <- lookupVariable varNameOfEVar
      case maybeVar of
        Just (varType, llvmVarName) -> do
          case varType of
            String -> do
              if isItAssignment then do
                updateVariableInStack varName varType (removeLeadingPercent exprString)
                addPhiNodeToFrame varName varType (removeLeadingPercent exprString)
              else do
                inIfElse <- isAnyTokenIfElseUp
                when inIfElse $ do
                  potentiallyAddDeclaredVariableToFrame varName varType (removeLeadingPercent exprString)
                addVariableToFrame varName varType (removeLeadingPercent exprString)
            _ -> do
              addCounter <- getNextVariableAndUpdate
              let addInstr = "%" ++  addCounter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, %" ++ llvmVarName
              if isItAssignment then do
                updateVariableInStack varName varType ( addCounter)
                addPhiNodeToFrame varName varType ( addCounter)
              else do
                inIfElse <- isAnyTokenIfElseUp
                when inIfElse $ do
                  potentiallyAddDeclaredVariableToFrame varName varType addCounter
                addVariableToFrame varName varType ( addCounter)
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
    else do
      if isItAssignment then do
        updateVariableInStack varName varType (removeLeadingPercent exprString)
        addPhiNodeToFrame varName varType (removeLeadingPercent exprString)
      else do
        inIfElse <- isAnyTokenIfElseUp
        when inIfElse $ do
          potentiallyAddDeclaredVariableToFrame varName varType (removeLeadingPercent exprString)
        addVariableToFrame varName varType (removeLeadingPercent exprString)
  _ -> do
    if isItAssignment then do
      updateVariableInStack varName varType (removeLeadingPercent exprString)
      addPhiNodeToFrame varName varType (removeLeadingPercent exprString)
    else do
      inIfElse <- isAnyTokenIfElseUp
      when inIfElse $ do
        potentiallyAddDeclaredVariableToFrame varName varType (removeLeadingPercent exprString)
      addVariableToFrame varName varType (removeLeadingPercent exprString)

declareEmptyStringIfNotExists :: LCS String
declareEmptyStringIfNotExists = do
  s <- get
  case Map.lookup "" (stringPool s) of
    Just label -> return ("@str"++show label)
    Nothing -> do
      stringId <- findOrDeclareString ""
      let strLabel = "@str" ++ show stringId
      let llvmString = convertToLlvmString ""
      let declaration = strLabel ++ " = private unnamed_addr constant [ 1 x i8] c\"" ++ llvmString ++ "\""
      modify $ \s -> s {compilerOutput = declaration : compilerOutput s}
      return strLabel

commonDecrIncrOperation :: Latte.Abs.Ident -> String -> StateT CompilerState (Either CompilerError) ()
commonDecrIncrOperation ident op = do
  s <- get
  let varName = name ident
  maybeVar <- lookupVariable varName
  case maybeVar of
    Just (varType, llvmVarName) -> do
      opCounter <- getNextVariableAndUpdate
      let opInstr = "%" ++ opCounter ++ " = " ++ op ++ " " ++ typeToLlvmKeyword varType ++ " %" ++ llvmVarName ++ ", 1"
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [opInstr] }
      updateVariableInStack varName varType opCounter
      addPhiNodeToFrame varName varType opCounter
    Nothing -> throwError $ "Variable not defined: " ++ varName

tokenIfElseUp :: LCS ()
tokenIfElseUp = do
  s <- get
  modify $ \s -> s { tokenIfElse = tokenIfElse s + 1 }

tokenIfElseDown :: LCS ()
tokenIfElseDown = do
  s <- get
  modify $ \s -> s { tokenIfElse = tokenIfElse s - 1 }

isAnyTokenIfElseUp :: LCS Bool
isAnyTokenIfElseUp = do
  s <- get
  return $ tokenIfElse s > 0

----------------
-- PHI SEGMENT--
----------------
pushPhiNodesFrame :: LCS ()
pushPhiNodesFrame = modify $ \s -> s { phiNodesStack = Map.empty : phiNodesStack s }

popPhiNodesFrameAndMergeItIntoStack :: LCS ()
popPhiNodesFrameAndMergeItIntoStack = do
  topFrame <- gets $ \s -> case phiNodesStack s of
    (frame:_) -> frame
    _ -> Map.empty
  stack <- gets phiNodesStack
  case stack of
    [_] -> modify $ \s -> s { phiNodesStack = [topFrame] }
    (_:rest) -> do
      isAnyTokenWhileUp <- isAnyTokenWhileUp
      if isAnyTokenWhileUp then do
        mergeTopFrameInElseIf
      else do
        popPhiNodesFrame
        newStack <- gets phiNodesStack
        case newStack of
          (currentTopFrame:rest) -> do
            let mergedFrame = mergePhiFrames currentTopFrame topFrame
            modify $ \s -> s { phiNodesStack = mergedFrame : rest }
          _ -> return ()
    [] -> modify $ \s -> s { phiNodesStack = [topFrame] }

popPhiNodesFrame :: LCS ()
popPhiNodesFrame = modify $ \s -> s {
  phiNodesStack = case phiNodesStack s of
      (_:rest) -> rest
      [] -> []
  }

mergePhiFrames :: Map String (Type, String) -> Map String (Type, String) -> Map String (Type, String)
mergePhiFrames frame1 frame2 = Map.union frame2 frame1  -- frame2 has priority

getTopPhiFrame :: LCS (Map String (Type, String))
getTopPhiFrame = gets $ \s -> case phiNodesStack s of
  (topFrame:_) -> topFrame
  _ -> Map.empty

addPhiNodeToFrame :: String -> Type -> String -> LCS ()
addPhiNodeToFrame varName varType llvmVarName = do
  phiStack <- gets phiNodesStack
  case phiStack of
    (currentFrame:rest) -> do
      let updatedFrame = Map.insert varName (varType, llvmVarName) currentFrame
      modify $ \s -> s { phiNodesStack = updatedFrame : rest }
    _ -> return ()

getPhiNode :: String -> LCS (Maybe (Type, String))
getPhiNode varName = gets $ \s ->
  let search [] = Nothing
      search (frame:rest) = case Map.lookup varName frame of
        Nothing -> search rest
        justVar -> justVar
  in search (phiNodesStack s)

createPhiBlock :: [Map String (Type, String)] -> Map String (Type, String) -> String -> String -> LCS [String]
createPhiBlock framesBeforeLoop frameAfterLoop phiLabel whileBodyLabel = do
  let varsAfterLoop = Map.toList frameAfterLoop
  phiNodes <- mapM createPhiNodeForVar varsAfterLoop
  return $ catMaybes phiNodes
  where
    createPhiNodeForVar (varName, (varType, varRegAfter)) = do
      let varBefore = findInPhiFrameByNameAndType varName varType framesBeforeLoop
      case varBefore of
        Just varRegBefore -> Just <$> createPhiNode varName varType varRegBefore varRegAfter phiLabel whileBodyLabel
        Nothing -> return Nothing

findInPhiFrameByNameAndType :: String -> Type -> [Map String (Type, String)] -> Maybe String
findInPhiFrameByNameAndType name typeToFind = go
  where
    go [] = Nothing
    go (frame:rest) =
      case Map.lookup name frame of
        Just (varType, regName) | varType == typeToFind -> Just regName
        _ -> go rest

createPhiNode :: String -> Type -> String -> String -> String -> String -> StateT CompilerState (Either CompilerError) String
createPhiNode varName varType regBefore regAfter labelBefore labelAfter = do
  labelCounter <- getNextPhiCounterAndUpdate
  updateVariableInStack varName varType ("phi_value_" ++ show labelCounter)
  updateVariableInPhiTopFrame varName varType ("phi_value_" ++ show labelCounter)
  return ("%" ++ "phi_value_" ++ show labelCounter ++ " = phi " ++
    typeToLlvmKeyword varType ++ " [ %" ++ regBefore ++ ", %" ++
    labelBefore ++ " ], [ %" ++ regAfter ++ ", %" ++ labelAfter ++ " ]")

createPhiNodeWithOneValue :: String -> Type -> String -> String -> StateT CompilerState (Either CompilerError) String
createPhiNodeWithOneValue varName varType reg label = do
  labelCounter <- getNextPhiCounterAndUpdate
  updateVariableInStack varName varType ("phi_value_" ++ show labelCounter)
  updateVariableInPhiTopFrame varName varType ("phi_value_" ++ show labelCounter)
  return ("%" ++ "phi_value_" ++ show labelCounter ++ " = phi " ++
    typeToLlvmKeyword varType ++ " [ %" ++ reg ++ ", %" ++
    label ++ " ]")

createSinglePhiBlock :: [Map String (Type, String)] -> Map String (Type, String) -> String -> LCS [String]
createSinglePhiBlock framesBeforeLoop frameAfterLoop whileCondLabel = do
  let varsAfterLoop = Map.toList frameAfterLoop
  phiNodes <- mapM createPhiNodeForVar varsAfterLoop
  return $ catMaybes phiNodes
  where
    createPhiNodeForVar (varName, (varType, varRegAfter)) = do
      let varBefore = findInPhiFrameByNameAndType varName varType framesBeforeLoop
      case varBefore of
        Just varRegBefore -> Just <$> createPhiNodeWithOneValue varName varType varRegAfter whileCondLabel
        Nothing -> return Nothing

updateVariableInPhiTopFrame :: String -> Type -> String -> LCS ()
updateVariableInPhiTopFrame  varName varType newReg = do
  s <- get
  let (currentFrame:rest) = phiNodesStack s
  let updatedFrame = Map.insert varName (varType, newReg) currentFrame
  put s { phiNodesStack = updatedFrame : rest }

updatePhiLabelCounterByOffset :: Int -> LCS ()
updatePhiLabelCounterByOffset offset = do
  s <- get
  let newCounter = phiCounter s + offset
  put s { phiCounter = newCounter }

fakeWhileRunAndCountPhiNodesAndAddPhiBlock :: Latte.Abs.Expr -> Latte.Abs.Stmt -> Int -> Int -> LCS Int
fakeWhileRunAndCountPhiNodesAndAddPhiBlock expr stmt labelCounter phiCounterOffset = do
  s <- get
  updatePhiLabelCounterByOffset phiCounterOffset
  phiLabel <- getTopLabel
  pushLabelToStack ("while_body_" ++ show labelCounter)
  originalReturnFlag <- gets returnReached
  modify $ \s -> s { returnReached = False }
  e <- compilerExpr expr
  compile stmt
  whileBodyLabel <- getTopLabel
  popLabelFromStack
  nodeStackAfterLoop <- gets phiNodesStack
  put s { phiNodesStack = nodeStackAfterLoop}
  variablesStackBeforeLoop <- gets variablesStack
  case nodeStackAfterLoop of
    (frameAfterLoop:_) -> do
      phiBlock <- createPhiBlock variablesStackBeforeLoop frameAfterLoop phiLabel whileBodyLabel
      modify $ \s -> s { compilerOutput = compilerOutput s ++ phiBlock }
      let newOffset = length $ filter (/= "") phiBlock
      return newOffset
    _ -> do
      return 0

fakeWhileRun :: Latte.Abs.Expr -> Latte.Abs.Stmt -> Int -> StateT CompilerState (Either CompilerError) ()
fakeWhileRun expr stmt labelCounter = do
  s <- get
  offset <- fakeWhileRunAndCountPhiNodesAndAddPhiBlock expr stmt labelCounter 0
  when (offset > 0) $ do
    put s
    fakeWhileRunAndCountPhiNodesAndAddPhiBlock expr stmt labelCounter offset
    return ()

checkIfVarHasPhiNode :: String -> LCS Bool
checkIfVarHasPhiNode varName = do
  s <- get
  let frames = phiNodesStack s
  let topFrame = case frames of
        (topFrame:_) -> topFrame
        _ -> Map.empty
  return $ Map.member varName topFrame

handlePhiBlockAtIfElse :: Map String (Type, String) -> Map String (Type, String) -> String -> String -> Bool -> Bool -> LCS [String]
handlePhiBlockAtIfElse phiFrameAfterTrue phiFrameAfterFalse lastLabelInTrueBranch lastLabelInFalseBranch wasReturnInTB wasReturnInFB = do
  if (not (wasReturnInFB || wasReturnInTB)) then do
    commonElements <- forM (Map.toList phiFrameAfterTrue) $ \(varName, (varType, regAfterTrue)) -> do
      case Map.lookup varName phiFrameAfterFalse of
        Just (_, regAfterFalse) -> do
          phiNode <- createPhiNode varName varType regAfterTrue regAfterFalse lastLabelInTrueBranch lastLabelInFalseBranch
          return $ Just phiNode
        Nothing -> do
          regBefore <- getVariableNameFromStack varName
          phiNode <- createPhiNode varName varType regBefore regAfterTrue lastLabelInFalseBranch lastLabelInTrueBranch
          return $ Just phiNode
    remainingElements <- forM (Map.toList phiFrameAfterFalse) $ \(varName, (varType, regAfterFalse)) -> do
      case Map.lookup varName phiFrameAfterTrue of
        Just _ -> return Nothing
        Nothing -> do
          regBefore <- getVariableNameFromStack varName
          phiNode <- createPhiNode varName varType regBefore regAfterFalse lastLabelInTrueBranch lastLabelInFalseBranch
          return $ Just phiNode
    return $ catMaybes (commonElements ++ remainingElements)
  else do
    if wasReturnInTB then do
      forM (Map.toList phiFrameAfterFalse) $ \(varName, (varType, regAfterFalse)) -> do
        createPhiNodeWithOneValue varName varType regAfterFalse lastLabelInFalseBranch
    else do
      forM (Map.toList phiFrameAfterTrue) $ \(varName, (varType, regAfterTrue)) -> do
        createPhiNodeWithOneValue varName varType regAfterTrue lastLabelInTrueBranch

createPhiNodesAndUpdateFramesWithExprsAfterCondElse :: String -> String -> [(Latte.Abs.Expr, String)] -> [(Latte.Abs.Expr, String)] ->
                                          LCS ([String], [(Latte.Abs.Expr, String)], [(Latte.Abs.Expr, String)])
createPhiNodesAndUpdateFramesWithExprsAfterCondElse ifLabel elseLabel ifFrame elseFrame = do
  (phiNodes, updatedIfFrame, updatedElseFrame) <- foldM processExpr ([], ifFrame, elseFrame) ifFrame
  return (phiNodes, updatedIfFrame, updatedElseFrame)
  where
    processExpr (accPhi, accIfFrame, accElseFrame) (expr, varNameIf) = do
      maybeVarNameElse <- searchExprsFrameAndLookupForExpr expr accElseFrame
      exprType <- getExpressionType expr
      case maybeVarNameElse of
        Just varNameElse ->
          if varNameIf /= varNameElse then do
            phiCounter <- getNextPhiCounterAndUpdate
            let phiLabel = "phi_value_" ++ show phiCounter
            let phiNode = "%" ++ phiLabel ++ " = phi " ++ typeToLlvmKeyword exprType ++ " [ %" ++ varNameIf ++ ", %" ++ ifLabel ++ " ], [ %" ++ varNameElse ++ ", %" ++ elseLabel ++ "] "
            let updatedIfFrame = replaceExprVarName expr phiLabel accIfFrame
            let updatedElseFrame = replaceExprVarName expr phiLabel accElseFrame
            return (phiNode : accPhi, updatedIfFrame, updatedElseFrame)
          else
            return (accPhi, accIfFrame, accElseFrame)
        Nothing -> return (accPhi, accIfFrame, accElseFrame)
    replaceExprVarName expr newName = map (\(e, var) -> if e == expr then (e, newName) else (e, var))

searchExprsFrameAndLookupForExpr :: Latte.Abs.Expr -> [(Latte.Abs.Expr, String)] -> LCS (Maybe String)
searchExprsFrameAndLookupForExpr expr frame = do
  matches <- filterM (\(e, _) -> isExprEqual e expr) frame
  return $ fmap snd (listToMaybe matches)


-------------------------------
--INDUCTION VARIABLES SEGMENT--
-------------------------------
pushInductionVariablesFrame :: LCS ()
pushInductionVariablesFrame = modify $ \s -> s { inductionVariablesStack = Map.empty : inductionVariablesStack s }

popInductionVariablesFrame :: LCS ()
popInductionVariablesFrame = modify $ \s -> s {
  inductionVariablesStack = case inductionVariablesStack s of
      (_:rest) -> rest
      [] -> []
  }

addInductionVariableToFrame :: String -> Int -> LCS ()
addInductionVariableToFrame varName val = modify $ \s ->
  let (currentFrame:rest) = inductionVariablesStack s
  in s { inductionVariablesStack = Map.insert varName (True, val) currentFrame : rest }

getValueOfInductionVariable :: String -> LCS (Maybe Int)
getValueOfInductionVariable varName = do
  maybeVar <- lookupForInductionVariableInTopFrame varName
  case maybeVar of
    Just (_, val) -> return $ Just val
    Nothing -> return Nothing

potentiallyMarkAsInductionVar :: String -> Int -> LCS ()
potentiallyMarkAsInductionVar varName val = do
  lookupResult <- lookupForInductionVariableInTopFrame varName
  case lookupResult of
    Just (True, _)  -> markVariableAsNotInduction varName
    Nothing -> do
      addInductionVariableToFrame varName val
      return ()
    _ -> return ()

markVariableAsNotInduction :: String -> LCS ()
markVariableAsNotInduction varName = modify $ \s ->
  let (currentFrame:rest) = inductionVariablesStack s
  in s { inductionVariablesStack = Map.insert varName (False,0) currentFrame : rest}

getTopInductionVariablesFrame :: LCS (Map String (Bool, Int))
getTopInductionVariablesFrame = gets $ \s -> case inductionVariablesStack s of
  (topFrame:_) -> topFrame
  _ -> Map.empty

lookupForInductionVariableInTopFrame :: String -> LCS (Maybe (Bool, Int))
lookupForInductionVariableInTopFrame varName = do
  topFrame <- getTopInductionVariablesFrame
  case Map.lookup varName topFrame of
    Just var -> return $ Just var
    Nothing -> return Nothing

analyzeStmtInLookingForIV :: Latte.Abs.Stmt  -> LCS ()
analyzeStmtInLookingForIV stmt = do
  case stmt of
    Latte.Abs.BStmt _ block -> analyzeBlockInLookingForIV  block
    Latte.Abs.Ass _ ident expr -> analyzeAssignmentInLookingForIV  ident expr
    Latte.Abs.Incr _ ident -> potentiallyMarkAsInductionVar (name ident) 1
    Latte.Abs.Decr _ ident -> potentiallyMarkAsInductionVar (name ident) (-1)
    Latte.Abs.Cond _ expr stmt -> do
      case expr of
        Latte.Abs.ELitTrue _ -> analyzeStmtInLookingForIV stmt
        Latte.Abs.ELitFalse _ -> return ()
        _ -> markAllVarsAsNonInductive stmt
    Latte.Abs.CondElse _ e1 stmt1 stmt2 -> case e1 of
        Latte.Abs.ELitTrue _ -> analyzeStmtInLookingForIV stmt1
        Latte.Abs.ELitFalse _ -> analyzeStmtInLookingForIV stmt2
        _ -> do
          markAllVarsAsNonInductive stmt1
          markAllVarsAsNonInductive stmt2
    Latte.Abs.While _ expr blockStmt -> case expr of
        Latte.Abs.ELitFalse _ -> return ()
        _ -> do
          markAllVarsAsNonInductive blockStmt
    _ -> return ()
    where
    markAllVarsAsNonInductive :: Latte.Abs.Stmt -> LCS ()
    markAllVarsAsNonInductive orgStmt =
      case orgStmt of
        Latte.Abs.BStmt _ (Latte.Abs.Block _ stmts)  -> mapM_ markAllVarsAsNonInductive stmts
        Latte.Abs.Cond _ expr stmt  -> markAllVarsAsNonInductive stmt
        Latte.Abs.CondElse _ expr stmt1 stmt2  -> do
          markAllVarsAsNonInductive stmt1
          markAllVarsAsNonInductive stmt2
        Latte.Abs.While _ expr stmt  -> markAllVarsAsNonInductive stmt
        Latte.Abs.Ass _ ident _ -> markVariableAsNotInduction (name ident)
        Latte.Abs.Incr _ ident -> markVariableAsNotInduction (name ident)
        Latte.Abs.Decr _ ident -> markVariableAsNotInduction (name ident)
        _ -> return ()

analyzeBlockInLookingForIV  :: Latte.Abs.Block  -> LCS ()
analyzeBlockInLookingForIV  (Latte.Abs.Block _ stmts) = mapM_ analyzeStmtInLookingForIV  stmts

analyzeAssignmentInLookingForIV  :: Latte.Abs.Ident -> Latte.Abs.Expr -> LCS ()
analyzeAssignmentInLookingForIV ident expr  = case expr of
    -- x = x + c
    Latte.Abs.EAdd _ expr1 op expr2 ->
      analyzeAddExpr ident expr1 op expr2
    --x = x
    Latte.Abs.EVar _ newIdent -> return ()
    _ -> do
      markVariableAsNotInduction (name ident)

analyzeAddExpr :: Latte.Abs.Ident -> Latte.Abs.Expr -> Latte.Abs.AddOp ->Latte.Abs.Expr -> LCS ()
analyzeAddExpr ident expr1 op expr2 = do
  isExpr1PotentiallyInduction <- analyzeExprForInduction ident expr1
  isExpr2PotentiallyInduction <- analyzeExprForInduction ident expr2
  if isExpr1PotentiallyInduction then do
    case expr2 of
      Latte.Abs.EVar _ newIdent -> do
        if newIdent == ident then
          case op of
            Latte.Abs.Plus _ -> potentiallyMarkAsInductionVar (name ident) (convertELitIntToInt expr1)
            Latte.Abs.Minus _ -> markVariableAsNotInduction (name ident)
        else markVariableAsNotInduction (name ident)
      _ -> markVariableAsNotInduction (name ident)
  else do
    if isExpr2PotentiallyInduction then do
      case expr1 of
        Latte.Abs.EVar _ newIdent -> do
          isNewIdentInduction <- checkIfThisVarIsInduction (name newIdent)
          if newIdent == ident then
            case op of
              Latte.Abs.Plus _ -> potentiallyMarkAsInductionVar (name ident) (convertELitIntToInt expr2)
              Latte.Abs.Minus _ -> potentiallyMarkAsInductionVar (name ident) (-(convertELitIntToInt expr2))
          else markVariableAsNotInduction (name ident)
        _ -> markVariableAsNotInduction (name ident)
    else do
      markVariableAsNotInduction (name ident)

analyzeExprForInduction :: Latte.Abs.Ident -> Latte.Abs.Expr -> LCS Bool
analyzeExprForInduction ident expr = case expr of
    Latte.Abs.ELitInt _ _ -> return True
    Latte.Abs.Neg _ expr -> analyzeExprForInduction ident expr
    Latte.Abs.EAdd _ expr1 op expr2 -> do
      res1 <- analyzeExprForInduction ident expr1
      res2 <- analyzeExprForInduction ident expr2
      return (res1 && res2)
    Latte.Abs.EMul _ expr1 op expr2 -> do
      res1 <- analyzeExprForInduction ident expr1
      res2 <- analyzeExprForInduction ident expr2
      return (res1 && res2)
    _ -> return False

checkIfThisVarIsInduction :: String -> LCS Bool
checkIfThisVarIsInduction varName = do
  topFrame <- getTopInductionVariablesFrame
  case Map.lookup varName topFrame of
    Just (True, _) -> return True
    _ -> return False

formatInductionVariables :: LCS String
formatInductionVariables = do
  stack <- gets inductionVariablesStack
  case stack of
      (frame:_) -> formatFrame frame
      _ -> return "No induction variables in this loop"
  where
      formatFrame :: Map String (Bool, Int) -> LCS String
      formatFrame frame = do
          let inductionVars = Map.toList $ Map.filter (fst) frame
          if null inductionVars
              then return "No induction variables in this loop"
              else do
                  varsFormatted <- mapM formatVar inductionVars
                  return $ intercalate ", " varsFormatted
      formatVar :: (String, (Bool, Int)) -> LCS String
      formatVar (name, (_, change)) = do
        registerOfVar <- lookupVariable name
        case registerOfVar of
          Just (_, reg) -> return $ "("++reg ++ " (" ++ name ++ ") changes by " ++ show change ++ " in each iteration)"
          Nothing -> throwError $ "Variable not defined: " ++ name ++ " (in formatInductionVariables)"

--------------------------
--REDUCE EXPR SUBSEGMENT--
--------------------------
reduceExpr :: Latte.Abs.Expr -> LCS Latte.Abs.Expr
reduceExpr node = case node of
  Latte.Abs.EAdd p l op r -> do
    rL <- reduceExpr l
    rR <- reduceExpr r
    return $ Latte.Abs.EAdd p rL op rR
  Latte.Abs.EMul p l op r -> reduceMul (Latte.Abs.EMul p l op r) node
  Latte.Abs.Neg p expr -> do
      case expr of
          Latte.Abs.ELitInt _ i -> return $ Latte.Abs.ELitInt p (-i)
          _ -> do
            r <- reduceExpr expr
            return $ Latte.Abs.Neg p r
  Latte.Abs.EAnd p l r -> do
    rL <- reduceExpr l
    rR <- reduceExpr r
    return $ Latte.Abs.EAnd p rL rR
  Latte.Abs.EOr p l r -> do
    rL <- reduceExpr l
    rR <- reduceExpr r
    return $ Latte.Abs.EOr p rL rR
  Latte.Abs.Not p expr -> do
    r <- reduceExpr expr
    return $ Latte.Abs.Not p r
  Latte.Abs.ERel p l op r -> do
    rL <- reduceExpr l
    rR <- reduceExpr r
    return $ Latte.Abs.ERel p rL op rR
  Latte.Abs.EApp p ident exprs -> do
    rExprs <- mapM reduceExpr exprs
    return $ Latte.Abs.EApp p ident rExprs
  _ -> return node

reduceMul :: Latte.Abs.Expr -> Latte.Abs.Expr -> LCS Latte.Abs.Expr
reduceMul (Latte.Abs.EMul p l op r) node = do
  reducedL <- reduceExpr l
  reducedR <- reduceExpr r
  case op of
    Latte.Abs.Times _ -> do
      case (reducedL, reducedR) of
        (Latte.Abs.ELitInt _ l, Latte.Abs.EVar _ r) -> reduceMulWithInduction reducedL reducedR node
        (Latte.Abs.EVar _ l, Latte.Abs.ELitInt _ r) -> reduceMulWithInduction reducedR reducedL node
        (Latte.Abs.ELitInt _ l, Latte.Abs.ELitInt _ r) -> do
          let number = l * r
          return $ Latte.Abs.ELitInt p number
        _ -> return $ Latte.Abs.EMul p reducedL op reducedR
    _ -> return $ Latte.Abs.EMul p reducedL op reducedR

reduceMulWithInduction :: Latte.Abs.Expr -> Latte.Abs.Expr -> Latte.Abs.Expr -> LCS Latte.Abs.Expr
reduceMulWithInduction (Latte.Abs.ELitInt p1 l) (Latte.Abs.EVar p2 r) node = do
  isIV <- checkIfThisVarIsInduction (name r)
  if isIV then do
    newVar <- createVariableWithExprToReduce node
    ivVal <- getValueOfInductionVariable (name r)
    valueToNewVar <- case ivVal of
      Just val -> do
        let number = fromIntegral val * (fromIntegral l)
        addVariableWithReducedExprToFrame newVar (name r) number
        return ()
      Nothing -> error "Something went wrong with reduceMulWithInduction"
    return (Latte.Abs.EVar p1 (Latte.Abs.Ident newVar))
  else return node

replaceExprsInStmtToReduced :: Latte.Abs.Stmt -> LCS (Maybe Latte.Abs.Stmt)
replaceExprsInStmtToReduced stmt =
  case stmt of
    Latte.Abs.Empty _ -> return Nothing
    Latte.Abs.BStmt p block -> do
      listOfStmts <- replaceExprsToReducedInBlock block
      let newBlock = convertListOfStmtToBlockBody block (catMaybes listOfStmts)
      return $ Just $ Latte.Abs.BStmt p newBlock
    Latte.Abs.Decl p t items -> do
      newItems <- forM items $ \item -> case item of
        Latte.Abs.Init pos ident expr -> do
          newExpr <- reduceExpr expr
          return $ Latte.Abs.Init pos ident newExpr
        noInit -> return noInit
      return $ Just $ Latte.Abs.Decl p t newItems
    Latte.Abs.Ass p ident e -> do
      let varName = name ident
      isIV <- checkIfThisVarIsInduction varName
      if isIV then do
        return (Just stmt)
      else do
        newExpr <- reduceExpr e
        return $ Just (Latte.Abs.Ass p ident newExpr)
    Latte.Abs.Cond p expr stms -> do
      case expr of
        Latte.Abs.ELitFalse _ -> return Nothing
        _ -> do
          newExpr <- reduceExpr expr
          newStmt <- replaceExprsInStmtToReduced stms
          return (Just (Latte.Abs.Cond p newExpr (fromMaybe stmt newStmt)))
    Latte.Abs.CondElse p e stmt1 stmt2 -> do
      case e of
        Latte.Abs.ELitTrue _ -> replaceExprsInStmtToReduced stmt1
        Latte.Abs.ELitFalse _ -> replaceExprsInStmtToReduced stmt2
        _ -> do
          newExpr <- reduceExpr e
          newStmt1 <- replaceExprsInStmtToReduced stmt1
          newStmt2 <- replaceExprsInStmtToReduced stmt2
          return $ Just $ Latte.Abs.CondElse p newExpr (fromMaybe stmt1 newStmt1) (fromMaybe stmt2 newStmt2)
    Latte.Abs.While p e s-> do
      newExpr <- reduceExpr e
      return $ Just $ Latte.Abs.While p newExpr s
    Latte.Abs.Incr p ident -> do
      return $ Just stmt
    Latte.Abs.Decr p ident -> do
      return $ Just stmt
    Latte.Abs.Ret p expr -> do
      newExpr <- reduceExpr expr
      return $ Just $ Latte.Abs.Ret p newExpr
    Latte.Abs.VRet p -> return $ Just stmt
    Latte.Abs.SExp p expr -> do
        newExpr <- reduceExpr expr
        return $ Just $ Latte.Abs.SExp p newExpr

replaceExprsToReducedInBlock  :: Latte.Abs.Block  -> LCS  ([Maybe Latte.Abs.Stmt])
replaceExprsToReducedInBlock  (Latte.Abs.Block _ stmts) = do
  mapM replaceExprsInStmtToReduced stmts

replaceBodyOfWhileWithReduction :: Latte.Abs.Stmt -> LCS (Maybe Latte.Abs.Stmt)
replaceBodyOfWhileWithReduction body = do
  analyzeStmtInLookingForIV body
  bodyWithReduction <- replaceExprsInStmtToReduced body
  case bodyWithReduction of
    Just (Latte.Abs.BStmt p block) -> do
      newBlock <- addAssignmentToVarsAfterIV block
      return $ Just $ Latte.Abs.BStmt p newBlock
    _ -> return $ Just body

addAssignmentToVarsAfterIV :: Latte.Abs.Block -> LCS Latte.Abs.Block
addAssignmentToVarsAfterIV block  = do
  stmtsToAddAfterIV <- createAssignmentBlockWithReducedExpression
  let mapWithIvsAndStmts = createMapFromList stmtsToAddAfterIV
  return $ addStmtsAfterInductionVars mapWithIvsAndStmts block

addStmtsAfterInductionVars :: Map.Map String [Latte.Abs.Stmt] -> Latte.Abs.Block -> Latte.Abs.Block
addStmtsAfterInductionVars stmtMap (Latte.Abs.Block p stmts) = Latte.Abs.Block p (addStmts stmts)
  where
    addStmts :: [Latte.Abs.Stmt] -> [Latte.Abs.Stmt]
    addStmts [] = []
    addStmts (s:ss) =
      case s of
        Latte.Abs.Ass p ident _ ->
          s : getExtraStmts (name ident) ++ addStmts ss
        Latte.Abs.Incr p ident ->
          s : getExtraStmts (name ident) ++ addStmts ss
        Latte.Abs.Decr p ident ->
          s : getExtraStmts (name ident) ++ addStmts ss
        Latte.Abs.BStmt p block ->
          Latte.Abs.BStmt p (addStmtsAfterInductionVars stmtMap block) : addStmts ss
        _ -> s : addStmts ss

    getExtraStmts :: String -> [Latte.Abs.Stmt]
    getExtraStmts varName = Map.findWithDefault [] varName stmtMap

----------------------------
--INLINE FUNCTIONS SEGMENT--
----------------------------
isFunctionInline :: (Latte.Abs.Ident, [Type]) -> LCS Bool
isFunctionInline function = do
  gets (Map.member function . inlineFunctions)

getArgumentRegisters :: Latte.Abs.Expr -> LCS String
getArgumentRegisters expr = do
  case expr of
    Latte.Abs.ELitInt _ val -> do
      counter <- getNextVariableAndUpdate
      let addInstr = "%" ++  counter ++ " = add i32 0, " ++ show val
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
      return ("%" ++  counter)
    Latte.Abs.ELitTrue _ -> do
      counter <- getNextVariableAndUpdate
      let addInstr = "%" ++  counter ++ " = add i1 0, 1"
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
      return ("%" ++  counter)
    Latte.Abs.ELitFalse _ -> do
      counter <- getNextVariableAndUpdate
      let addInstr = "%" ++  counter ++ " = add i1 0, 0"
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
      return ("%" ++  counter)
    _ -> compilerExpr expr

pushInlineFunctionReturnFrame :: LCS ()
pushInlineFunctionReturnFrame =
  modify $ \s -> s {
    inlineFunctionsReturnStack = Map.empty : inlineFunctionsReturnStack s,
    inlineFunctionsToken = inlineFunctionsToken s + 1
    }

popInlineFunctionReturnFrame :: LCS ()
popInlineFunctionReturnFrame = modify $ \s -> s {
  inlineFunctionsReturnStack = case inlineFunctionsReturnStack s of
      (_:rest) -> rest
      [] -> [],
  inlineFunctionsToken = inlineFunctionsToken s - 1
}

addInlineFunctionReturnToFrame :: String -> String -> LCS ()
addInlineFunctionReturnToFrame labal reg = modify $ \s ->
  let (currentFrame:rest) = inlineFunctionsReturnStack s
  in s { inlineFunctionsReturnStack = Map.insert labal reg currentFrame : rest }

checkIfCodeIsInsideInlineFunction :: LCS Bool
checkIfCodeIsInsideInlineFunction = do
  token <- gets inlineFunctionsToken
  return $ token > 0

findLabelForThisInlineFunction :: LCS String
findLabelForThisInlineFunction = do
  labelCouter <- getNextLabelCounterAndUpdate
  let label = "end_inline_function_" ++ show labelCouter
  modify $ \s -> s { inlineFunctionsLabelsStack = label : inlineFunctionsLabelsStack s }
  return label

getEndOfActualFunctionLabel :: LCS String
getEndOfActualFunctionLabel = do
  stack <- gets inlineFunctionsLabelsStack
  let label = case stack of
        (top:_) -> top
        _ -> error "No label on stack"
  return label

inlineFunctionCall :: Latte.Abs.Ident -> [String] -> [Type] -> StateT CompilerState (Either CompilerError) String
inlineFunctionCall ident argsRegisters argsTypes = do
  originalReturnFlag <- gets returnReached
  orifinalExprsFrame <- gets computedExprsStack
  orifinalVariablesFrame <- gets variablesStack
  orifinalPhiNodesFrame <- gets phiNodesStack
  originalInlineFunctionsLabelsStack <- gets inlineFunctionsLabelsStack
  -- pushExprsFrame
  pushVariablesFrame
  pushInlineFunctionReturnFrame
  let functionName = name ident
  (retType,argsAbs,body) <- getInlineFunctionItems ident argsTypes
  originalType <- gets currentInlineFunctionType
  modify $ \s -> s { currentInlineFunctionType = retType, inlineReturnReached = False }
  addInlineFunctionArgumentsToFrame argsAbs argsRegisters argsTypes
  label <- findLabelForThisInlineFunction
  compile body
  topLabel <- getTopLabel
  pushLabelToStack label
  if retType == Void then do
    isBrLastStmt <- gets isBrLastStmt
    unless isBrLastStmt $ do
      dummy <- putDummyRegister
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ label] }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [label ++ ":"] }
    register <- putDummyRegister
    popLabelFromStack
    popVariablesFrame
    popInlineFunctionReturnFrame
    modify $ \s -> s { currentInlineFunctionType = originalType, returnReached = originalReturnFlag,
    computedExprsStack = orifinalExprsFrame,
    variablesStack = orifinalVariablesFrame, phiNodesStack = orifinalPhiNodesFrame, inlineFunctionsLabelsStack = originalInlineFunctionsLabelsStack }
    return register
  else do
    register <- handleReturnPhiBlockInInlineFunction retType label topLabel
    popVariablesFrame
    popInlineFunctionReturnFrame
    modify $ \s -> s { currentInlineFunctionType = originalType, returnReached = originalReturnFlag,
    computedExprsStack = orifinalExprsFrame,
    variablesStack = orifinalVariablesFrame, phiNodesStack = orifinalPhiNodesFrame, inlineFunctionsLabelsStack = originalInlineFunctionsLabelsStack }
    return register

getInlineFunctionItems :: Latte.Abs.Ident -> [Type] -> StateT      CompilerState      (Either CompilerError)      (Type, [Latte.Abs.Arg], Latte.Abs.Block)
getInlineFunctionItems ident args = do
  inlineFunctions <- gets inlineFunctions
  case Map.lookup (ident, args) inlineFunctions of
    Just all -> return all
    Nothing -> throwError $ "Function " ++ show ident ++ " with args " ++ show args ++ " not found for inlining"

addInlineFunctionArgumentsToFrame :: [Latte.Abs.Arg] -> [String] -> [Type] -> LCS ()
addInlineFunctionArgumentsToFrame functionArgs argsRegisters argsTypes = do
  let argNames = map (\(Latte.Abs.Arg _ _ argIdent) -> name argIdent) functionArgs
  let argsRegistersWithoutPercent = map removeLeadingPercent argsRegisters
  let argNameTypeRegTuples = zip3 argNames argsTypes argsRegistersWithoutPercent
  forM_ argNameTypeRegTuples $ \(argName, argType, argsRegistersWithoutPercent) -> do
    addVariableToFrame argName argType argsRegistersWithoutPercent
    addPhiNodeToFrame argName argType argsRegistersWithoutPercent

handleReturnPhiBlockInInlineFunction :: Type -> String -> String -> LCS String
handleReturnPhiBlockInInlineFunction functionType labelAtTheEnd oneLabelBefore = do
  isBrLastStmt <- gets isBrLastStmt
  unless isBrLastStmt $ do
    dummy <- putDummyRegister
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ labelAtTheEnd] }
    addInlineFunctionReturnToFrame oneLabelBefore dummy
  modify $ \s -> s { compilerOutput = compilerOutput s ++ [labelAtTheEnd ++ ":"] }
  counter <- getNextVariableAndUpdate
  topFrame <- gets $ \s -> case inlineFunctionsReturnStack s of
    (frame:_) -> frame
    _ -> Map.empty
  case Map.toList topFrame of
    [] -> return ""
    [(label, reg)] -> do
      case functionType of
        String -> do
          emptyString <- declareEmptyStringIfNotExists
          nextCounter <- getNextVariableAndUpdate
          let callOfEmptyString = "%" ++ counter ++ " = getelementptr inbounds [1 x i8], [1 x i8]* " ++ emptyString ++ ", i32 0, i32 0"
          let callOfConcat = "%" ++ nextCounter ++ " = call i8* @doNotUseThatNameConcat(i8* " ++ reg ++ ", i8* %" ++ counter ++ ")"
          concatWasDeclared <- gets concatWasDeclared
          unless concatWasDeclared $ do
            modify $ \s -> s { compilerOutput = "declare i8* @doNotUseThatNameConcat(i8*, i8*)" : compilerOutput s }
            modify $ \s -> s { concatWasDeclared = True }
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [callOfEmptyString, callOfConcat] }
          return ("%" ++ nextCounter)
        _ -> do
          let call = "%" ++ counter ++ " = add " ++ typeToLlvmKeyword functionType ++ " 0, " ++ reg
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [call] }
          return ("%" ++ counter)
    entries -> do
      let phiEntries = map (\(label, reg) -> "[ " ++ reg ++ ", %" ++ label ++ " ]") entries
      let phiString = intercalate ", " phiEntries
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ counter ++ " = phi " ++ typeToLlvmKeyword functionType ++ " " ++ phiString] }
      return ("%" ++ counter)

generateBeginOfInlineFunctionComment key argRegisters = do
  inlineFunctions <- gets inlineFunctions
  let maybeFunc = Map.lookup key inlineFunctions
  case maybeFunc of
    Just (_, args, _) -> do
      let argNames = map (\(Latte.Abs.Arg _ _ ident) -> Latte.Helpers.name ident) args
      let argInfo = zipWith (\name reg -> name ++ ": " ++ reg) argNames argRegisters
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["; Inline function call: " ++ functionName key] }
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["; Arguments: " ++ intercalate ", " argInfo] }

generateEndOfInlineFunctionComment key retType reg = do
  case retType of
    Void -> modify $ \s -> s { compilerOutput = compilerOutput s ++ ["; End of inline function call: " ++ functionName key] }
    _ ->  modify $ \s -> s { compilerOutput = compilerOutput s ++ ["; End of inline function call: " ++ functionName key ++ ", with result in register " ++ reg] }

putDummyRegister :: LCS String
putDummyRegister = do
  isItInInlineFunction <- checkIfCodeIsInsideInlineFunction
  if isItInInlineFunction then
    generateDummyRegister
  else return ""

generateDummyRegister :: LCS String
generateDummyRegister = do
  counter <- getNextVariableAndUpdate
  let dummyRegister = "%" ++ counter ++ "_dummy"
  returnType <- gets currentInlineFunctionType
  case returnType of
    String -> do
      emptyStringLabel <- declareEmptyStringIfNotExists
      let call = dummyRegister ++ " = getelementptr inbounds [1 x i8], [1 x i8]* " ++
            emptyStringLabel ++ ", i32 0, i32 0"
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [call] }
    Void -> do
      let dummyAdd = dummyRegister ++ " = add i1 0, 0"
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [dummyAdd] }
    _ -> do
      let dummyAdd = dummyRegister ++ " = add " ++ typeToLlvmKeyword returnType ++ " 0, 0"
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [dummyAdd] }
  return dummyRegister

-------------------------------------------------------
------------------EXPRESSIONS SECTION------------------
-------------------------------------------------------
class CompileExpr a where
  compilerExpr :: a -> LCS String

instance CompileExpr Latte.Abs.Expr where
  compilerExpr  node = case node of
    Latte.Abs.ELitInt _ val -> return (show val)
    Latte.Abs.ELitTrue _ -> return "1"
    Latte.Abs.ELitFalse _ -> return "0"
    Latte.Abs.EString _ str -> do
      lookupExpr <- lookupExprsStack node
      case lookupExpr of
        Just llvmVarName -> return $ "%" ++ llvmVarName
        Nothing -> do
          s <- get
          (strLabel, isNew) <- case Map.lookup str (stringPool s) of
            Just label -> return ("@str" ++ show label, False)
            Nothing -> do
              stringId <- findOrDeclareString str
              return ("@str" ++ show stringId, True)
          let llvmString = convertToLlvmString str
          let stringLength = length str + 1
          nextIndirectVariable <- getNextVariableAndUpdate
          let call = "%"++  nextIndirectVariable ++ " = getelementptr inbounds [" ++ show stringLength ++
                    " x i8], [" ++ show stringLength ++ " x i8]* " ++ strLabel ++ ", i32 0, i32 0"
          when isNew $ do
            let declaration = strLabel ++ " = private unnamed_addr constant [" ++ show stringLength ++ " x i8] c\"" ++ llvmString ++ "\""
            modify $ \s -> s {compilerOutput = declaration : compilerOutput s}
          modify $ \s -> s {compilerOutput = compilerOutput s ++ [call]}
          addExprToFrame node ( nextIndirectVariable)
          return $ "%" ++  nextIndirectVariable
    Latte.Abs.EAdd p l op r -> do
      lookupExpr <- lookupExprsStack node
      case lookupExpr of
        Just llvmVarName -> do
          return $ "%" ++ llvmVarName
        Nothing -> do
          l' <- compilerExpr l
          r' <- compilerExpr r
          let op' = case op of
                Latte.Abs.Plus _ -> "add"
                Latte.Abs.Minus _ -> "sub"
          counter <- getNextVariableAndUpdate
          lType <- getExpressionType l
          case (lType, op) of
            (String, Latte.Abs.Plus _) -> do
              let call = "%" ++  counter ++ " = call i8* @doNotUseThatNameConcat(i8* " ++ l' ++ ", i8* " ++ r' ++ ")"
              concatWasDeclared <- gets concatWasDeclared
              unless concatWasDeclared $ do
                modify $ \s -> s { compilerOutput = "declare i8* @doNotUseThatNameConcat(i8*, i8*)" : compilerOutput s }
                modify $ \s -> s { concatWasDeclared = True }
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [call] }
              addExprToFrame node ( counter)
              return $ "%" ++ counter
            (String, _) -> throwError ("Cannot subtract strings, only concatenation is allowed, at " ++ errLocation p)
            (Integer, _) -> do
              let op' = case op of
                    Latte.Abs.Plus _ -> "add"
                    Latte.Abs.Minus _ -> "sub"
              modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++  counter ++ " = " ++ op' ++ " i32 " ++ l' ++ ", " ++ r']}
              addExprToFrame node ( counter)
              return $ "%" ++  counter
            _ -> throwError $ "Cannot add two: " ++ show lType ++ " and " ++ show r ++ ", at " ++ errLocation p
    Latte.Abs.EMul p l op r -> do
      lookupExpr <- lookupExprsStack node
      case lookupExpr of
        Just llvmVarName -> return $ "%" ++ llvmVarName
        Nothing -> do
          l' <- compilerExpr l
          r' <- compilerExpr r
          let op' = case op of
                Latte.Abs.Times _ -> "mul"
                Latte.Abs.Div _ -> "sdiv"
                Latte.Abs.Mod _ -> "srem"
          counter <- getNextVariableAndUpdate
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++  counter ++ " = " ++ op' ++ " i32 " ++ l' ++ ", " ++ r']}
          addExprToFrame node ( counter)
          return $ "%" ++  counter
    Latte.Abs.Neg p expr -> do
      lookupExpr <- lookupExprsStack node
      case lookupExpr of
        Just llvmVarName -> return $ "%" ++ llvmVarName
        Nothing -> do
          e <- compilerExpr expr
          counter <- getNextVariableAndUpdate
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++  counter ++ " = sub i32 0, " ++ e] }
          addExprToFrame node ( counter)
          return $ "%" ++  counter
    Latte.Abs.EAnd p l r -> do
      lExpr <- compilerExpr l
      labelCounter <- getNextLabelCounterAndUpdate
      let trueLabel = "and_true_" ++ show labelCounter
      let falseLabel = "and_false_" ++ show labelCounter
      let endLabel = "and_end_" ++ show labelCounter
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ lExpr ++ ", label %" ++ trueLabel ++ ", label %" ++ falseLabel] }
      pushExprsFrame
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [trueLabel ++ ":"] }
      pushLabelToStack trueLabel
      rExpr <- compilerExpr r
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
      popExprsFrame
      pushExprsFrame
      trueLabelToPhi <- getTopLabel
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [falseLabel ++ ":"] }
      pushLabelToStack falseLabel
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
      popExprsFrame
      falseLabelToPhi <- getTopLabel
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
      pushLabelToStack endLabel
      resultVar <- getNextVariableAndUpdate
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++  resultVar ++ " = phi i1 [ " ++ rExpr ++ ", %" ++ trueLabelToPhi ++ " ], [ 0, %" ++ falseLabelToPhi ++ " ]"] }
      addExprToFrame node ( resultVar)
      return $ "%" ++  resultVar
    Latte.Abs.EOr p l r -> do
      lExpr <- compilerExpr l
      labelCounter <- getNextLabelCounterAndUpdate
      let falseLabel = "or_false_" ++ show labelCounter
      let trueLabel = "or_true_" ++ show labelCounter
      let endLabel = "or_end_" ++ show labelCounter
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ lExpr ++ ", label %" ++ trueLabel ++ ", label %" ++ falseLabel] }
      pushExprsFrame
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [trueLabel ++ ":"] }
      pushLabelToStack trueLabel
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
      popExprsFrame
      pushExprsFrame
      trueLabelToPhi <- getTopLabel
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [falseLabel ++ ":"] }
      pushLabelToStack falseLabel
      rExpr <- compilerExpr r
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
      popExprsFrame
      falseLabelToPhi <- getTopLabel
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
      pushLabelToStack endLabel
      resultVar <- getNextVariableAndUpdate
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++  resultVar ++ " = phi i1 [ 1, %" ++ trueLabelToPhi ++ " ], [ " ++ rExpr ++ ", %" ++ falseLabelToPhi ++ " ]"] }
      addExprToFrame node ( resultVar)
      return $ "%" ++  resultVar
    Latte.Abs.Not p expr -> do
      lookupExpr <- lookupExprsStack node
      case lookupExpr of
        Just llvmVarName -> return $ "%" ++ llvmVarName
        Nothing -> do
          e <- compilerExpr expr
          counter <- getNextVariableAndUpdate
          let notInstr = "%" ++  counter ++ " = xor i1 " ++ e ++ ", 1"
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [notInstr] }
          addExprToFrame node ( counter)
          return $ "%" ++  counter
    Latte.Abs.ERel p l op r -> do
      lookupExpr <- lookupExprsStack node
      case lookupExpr of
        Just llvmVarName -> return $ "%" ++ llvmVarName
        Nothing -> do
          lExpr <- compilerExpr l
          rExpr <- compilerExpr r
          lType <- getExpressionType l
          counter <- getNextVariableAndUpdate
          let relOp = case op of
                Latte.Abs.LTH _ -> "slt"
                Latte.Abs.LE _ -> "sle"
                Latte.Abs.GTH _ -> "sgt"
                Latte.Abs.GE _ -> "sge"
                Latte.Abs.EQU _ -> "eq"
                Latte.Abs.NE _ -> "ne"
          case lType of
            Integer -> do
              let relInstr = "%" ++  counter ++ " = icmp " ++ relOp ++ " i32 " ++ lExpr ++ ", " ++ rExpr
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [relInstr] }
              addExprToFrame node ( counter)
              return $ "%" ++  counter
            Boolean -> do
              let relInstr = "%" ++  counter ++ " = icmp " ++ relOp ++ " i1 " ++ lExpr ++ ", " ++ rExpr
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [relInstr] }
              addExprToFrame node ( counter)
              return $ "%" ++  counter
            String -> do
              nextCounter <- getNextVariableAndUpdate
              let callStrcmp = "%" ++  counter ++ " = call i32 @strcmp(i8* " ++ lExpr ++ ", i8* " ++ rExpr ++ ")"
              let icmpResult = "%" ++  nextCounter ++ " = icmp " ++ relOp ++ " i32 %" ++  counter ++ ", 0"
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [callStrcmp, icmpResult] }
              modify $ \s -> s { compilerOutput = "declare i32 @strcmp(i8*, i8*)" : compilerOutput s }
              addExprToFrame node ( nextCounter)
              return $ "%" ++  nextCounter
            _ -> throwError $ "Comparison not supported for types: " ++ show lType ++ " and " ++ show r ++ ", at " ++ errLocation p
    Latte.Abs.EVar p ident -> do
      s <- get
      let varName = name ident
      maybeVar <- lookupVariable varName
      case maybeVar of
        Just (varType, llvmVarName) -> do
          addExprToFrame node llvmVarName
          return $ "%" ++ llvmVarName
        Nothing -> throwError $ "Variable not defined: " ++ varName ++ ", " ++ errLocation p
    Latte.Abs.EApp p ident exprs -> do
      lookupExpr <- lookupExprsStack node
      case lookupExpr of
        Just llvmVarName -> return $ "%" ++ llvmVarName
        Nothing -> do
          s <- get
          argTypes <- mapM getExpressionType exprs
          let funType = case Map.lookup (ident, argTypes) (functionsSignatures s) of
                Just t -> t
                _ -> error $ "Function " ++ functionName (ident, argTypes) ++ " not found"
          isFnInline <- isFunctionInline (ident, argTypes)
          if isFnInline then do
            argRegisters <- mapM getArgumentRegisters exprs
            generateBeginOfInlineFunctionComment (ident, argTypes) argRegisters
            reg <- inlineFunctionCall ident argRegisters argTypes
            generateEndOfInlineFunctionComment (ident, argTypes) funType reg
            return reg
          else do
            argExprs <- mapM compilerExpr exprs
            let argsCall = intercalate ", " (zipWith (\ t e -> t ++ " " ++ e) (map typeToLlvmKeyword argTypes) argExprs)
            let funCall = "call " ++ typeToLlvmKeyword funType ++ " @" ++ name ident ++ "(" ++ argsCall ++ ")"
            case funType of
              Void -> do
                modify $ \s -> s { compilerOutput = compilerOutput s ++ [funCall] }
                return ""
              _ -> do
                counter <- getNextVariableAndUpdate
                let callInstr = "%" ++  counter ++ " = " ++ funCall
                modify $ \s -> s { compilerOutput = compilerOutput s ++ [callInstr]}
                addExprToFrame node ( counter)
                return $ "%" ++  counter

---------------------------------------------------
------------------COMPILE SECTION------------------
---------------------------------------------------
class Compile a where
  compile :: a -> LCS ()

instance Compile Latte.Abs.Program where
    compile (Latte.Abs.Program _ topdefs) = do
        forM_ topdefs compile

instance Compile Latte.Abs.TopDef where
  compile fndef@(Latte.Abs.FnDef p t ident args block) = do
    inlineFunctions <- gets inlineFunctions
    let argsTypes = map (\(Latte.Abs.Arg _ argType _) -> keywordToType argType) args
    if (Map.member (ident, argsTypes) inlineFunctions) then return ()
    else do
      pushVariablesFrame
      pushExprsFrame
      let retType = keywordToType t
      let argsStr = intercalate ", " (map printArg args)
      forM_ args $ \(Latte.Abs.Arg _ argType ident) -> do
        let varName = name ident
        let varType = keywordToType argType
        addVariableToFrame varName varType varName
        modify $ \s -> s {
          arguments = Map.insert varName varType (arguments s)
        }
      let funName = name ident
      let funSignature =  typeToLlvmKeyword retType ++ " @" ++ funName ++ "(" ++ argsStr ++ ")"
      let funHeader = "define " ++ funSignature
      modify $ \s -> s {
        compilerOutput = compilerOutput s ++ [funHeader, "{"] ++ ["entry:"],
        variablesCounter = 0,
        returnReached = False,
        phiNodesStack = []
      }
      pushLabelToStack "entry"
      variablesCounter <- gets variablesCounter
      compile block
      returnFlag <- gets returnReached
      when (not returnFlag && retType == Void) $ modify $ \s -> s { compilerOutput = compilerOutput s ++ ["ret void"] }
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["}"]}
      modify $ \s -> s { labelsStack = [], labelsUsedInReturn = [], variablesStack = [], computedExprsStack = [], phiNodesStack = [] }

instance Compile Latte.Abs.Block where
  compile (Latte.Abs.Block _ stmts) = do
    forM_ stmts $ \stmt -> do
      returnFlag <- gets returnReached
      unless returnFlag $ compile stmt

instance Compile Latte.Abs.Stmt where
  compile stmt = do
    modify $ \s -> s { isBrLastStmt = False }
    returnFlag <- gets returnReached
    if returnFlag
      then return ()
    else case stmt of
      Latte.Abs.Empty _ -> return ()
      Latte.Abs.BStmt _ block -> do
        pushVariablesFrame
        modify $ \s -> s { returnReached = False }
        compile block
        popVariablesFrame
      Latte.Abs.Decl p type_ items -> forM_ items $ \item -> case item of
        Latte.Abs.NoInit _ ident -> do
          let varName = name ident
          let varType = keywordToType type_
          counter <- getNextVariableAndUpdate
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["; Declaration without init: " ++ varName ++ " to register %" ++ counter] }
          case varType of
            String -> do
              emptyStringLabel <- declareEmptyStringIfNotExists
              let call = "%"++  counter ++ " = getelementptr inbounds [1 x i8], [1 x i8]* " ++
                    emptyStringLabel ++ ", i32 0, i32 0"
              addVariableToFrame varName varType ( counter)
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [call] }
            _ -> do
              let declToZero = "%" ++  counter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, 0"
              addVariableToFrame varName varType ( counter)
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [declToZero] }
        Latte.Abs.Init _ ident expr -> do
          let varName = name ident
          let varType = keywordToType type_
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["; Declaration with init: " ++ varName] }
          e <- compilerExpr expr
          fakeInitInsteadOfAlloca varName varType e expr False
      Latte.Abs.Ass p ident expr -> do
        s <- get
        let varName = name ident
        maybeVar <- lookupVariable varName
        case maybeVar of
          Just (varType, llvmVarName) -> do
            e <- compilerExpr expr
            removeExprsWithVarFromAllFrames varName
            exprWithType <- combineTypeAndIndentOfExpr expr e
            fakeInitInsteadOfAlloca varName varType e expr True
          Nothing -> throwError $ "Variable not defined: " ++ varName ++ ", " ++ errLocation p
      Latte.Abs.Cond p expr stmt -> do
        e <- compilerExpr expr
        case expr of
          Latte.Abs.ELitTrue _ -> compile stmt
          Latte.Abs.ELitFalse _ -> return ()
          _ -> do
            counter <- getNextLabelCounterAndUpdate
            topLabel <- getTopLabel
            let trueLabel = "if_true_" ++ show counter
            let endLabel = "if_end_" ++ show counter
            modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ e ++ ", label %" ++ trueLabel ++ ", label %" ++ endLabel] }
            varsInCaseOfReturnBeforeIf <- gets variablesStack
            phiInCaseOfReturnBeforeIf <- gets phiNodesStack
            pushExprsFrame
            pushLabelToStack trueLabel
            pushPhiNodesFrame
            framesBeforeIf <- gets variablesStack
            modify $ \s -> s { compilerOutput = compilerOutput s ++ [trueLabel ++ ":"] }
            originalReturnFlag <- gets returnReached
            modify $ \s -> s { returnReached = False }
            compile stmt
            returnFlag <- gets returnReached
            phiFrameAfterIf <- gets phiNodesStack
            if not returnFlag then do
              popPhiNodesFrameAndMergeItIntoStack
              modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
            else popPhiNodesFrame
            modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
            lastLabel <- getTopLabel
            pushLabelToStack endLabel
            modify $ \s -> s { returnReached = originalReturnFlag}
            popExprsFrame
            if not returnFlag then do
              case phiFrameAfterIf of
                (phiFrameAfter:_) -> do
                  phiBlock <- createPhiBlock framesBeforeIf phiFrameAfter topLabel lastLabel
                  modify $ \s -> s { compilerOutput = compilerOutput s ++ phiBlock }
                  if null phiBlock then do
                    isItInInlineFunction <- checkIfCodeIsInsideInlineFunction
                    when isItInInlineFunction $ do
                      dummy <- putDummyRegister
                      modify $ \s -> s { isBrLastStmt = False }
                      when returnFlag $ addInlineFunctionReturnToFrame endLabel dummy
                  else
                    modify $ \s -> s { isBrLastStmt = False }
                _ -> do
                  isItInInlineFunction <- checkIfCodeIsInsideInlineFunction
                  when isItInInlineFunction $ do
                    dummy <- putDummyRegister
                    modify $ \s -> s { isBrLastStmt = False }
                    when returnFlag $ addInlineFunctionReturnToFrame endLabel dummy
            else do
              modify $ \s -> s { variablesStack = varsInCaseOfReturnBeforeIf }
              modify $ \s -> s { phiNodesStack = phiInCaseOfReturnBeforeIf }
              isItInInlineFunction <- checkIfCodeIsInsideInlineFunction
              when isItInInlineFunction $ do
                dummy <- putDummyRegister
                modify $ \s -> s { isBrLastStmt = False }

      Latte.Abs.CondElse p expr stmt1 stmt2 -> do
        e <- compilerExpr expr
        case expr of
          Latte.Abs.ELitTrue _ -> compile stmt1
          Latte.Abs.ELitFalse _ -> compile stmt2
          _ -> do
            tokenIfElseUp
            counter <- getNextLabelCounterAndUpdate
            let trueLabel = "if_true_" ++ show counter
            let falseLabel = "if_false_" ++ show counter
            let endLabel = "if_end_" ++ show counter
            modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ e ++ ", label %" ++ trueLabel ++ ", label %" ++ falseLabel] }
            isItInInlineFunction <- checkIfCodeIsInsideInlineFunction
            -- true branch
            pushPhiNodesFrame
            pushDeclaredVariablesFrame
            pushLabelToStack trueLabel
            originalReturnFlag <- gets returnReached
            variableStackBeforeTrueBranch <- gets variablesStack
            exprsFrameBeforeTrueBranch <- gets computedExprsStack
            pushExprsFrame
            modify $ \s -> s { compilerOutput = compilerOutput s ++ [trueLabel ++ ":"] }
            modify $ \s -> s { returnReached = False }
            compile stmt1
            returnFlag1 <- gets returnReached
            exprFrameAfterTrueBranchBeforeDeleteDecls <- gets computedExprsStack
            declsInIf <- gets declaredVariablesInIfElseBlock
            tfAIf <- getTopFrameOfDeclaredVariables
            exprFrameAfterTrueBranch <- removeDeclaredVariablesFromExprsStack exprFrameAfterTrueBranchBeforeDeleteDecls tfAIf
            popDeclaredVariablesFrame
            popExprsFrame
            modify $ \s -> s { variablesStack = variableStackBeforeTrueBranch, computedExprsStack = exprsFrameBeforeTrueBranch }
            topLabelAfterTrueBranch <- getTopLabel
            phiFrameAfterTrueBranch <- getTopPhiFrame
            popPhiNodesFrame
            unless returnFlag1 $
              modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
            -- false branch
            pushDeclaredVariablesFrame
            variableStackBeforeFalseBranch <- gets variablesStack
            pushExprsFrame
            pushPhiNodesFrame
            pushLabelToStack falseLabel
            modify $ \s -> s { compilerOutput = compilerOutput s ++ [falseLabel ++ ":"] }
            modify $ \s -> s { returnReached = False }
            compile stmt2
            topLabelAfterFalseBranch <- getTopLabel
            exprFrameAfterFalseBranchBeforeDeleteDecls <- gets computedExprsStack
            declsInElse <- gets declaredVariablesInIfElseBlock
            tfAElse <- getTopFrameOfDeclaredVariables
            exprFrameAfterFalseBranch <- removeDeclaredVariablesFromExprsStack exprFrameAfterFalseBranchBeforeDeleteDecls tfAElse
            popDeclaredVariablesFrame
            phiWithExprsInBothFrames <- updateExprsStackAfterIfElse topLabelAfterTrueBranch topLabelAfterFalseBranch exprFrameAfterTrueBranch exprFrameAfterFalseBranch
            mergeTopFrameInElseIf
            modify $ \s -> s { variablesStack = variableStackBeforeFalseBranch}
            returnFlag2 <- gets returnReached
            phiFrameAfterFalseBranch <- getTopPhiFrame
            -- end
            unless (returnFlag1 && returnFlag2) $ do
              modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
              phiBlock <- handlePhiBlockAtIfElse phiFrameAfterTrueBranch phiFrameAfterFalseBranch topLabelAfterTrueBranch topLabelAfterFalseBranch returnFlag1 returnFlag2
              modify $ \s ->
                let outputWithPhiBlock = compilerOutput s ++ phiBlock
                    outputWithPhiAndExprs =
                        if not (null phiWithExprsInBothFrames)
                        then outputWithPhiBlock ++ ["; Expressions used in both branches of ifelse: "] ++ phiWithExprsInBothFrames
                        else outputWithPhiBlock
                in s { compilerOutput = outputWithPhiAndExprs }
              when (isItInInlineFunction && ((null phiWithExprsInBothFrames) && (null phiBlock))) do
                putDummyRegister
                modify $ \s -> s { isBrLastStmt = False }
              pushLabelToStack endLabel
              modify $ \s -> s { returnReached = originalReturnFlag}
            tokenIfElseDown
      Latte.Abs.While p expr stmt -> do
        counter <- getNextLabelCounterAndUpdate
        pushInductionVariablesFrame
        pushReduceStacks
        newBody <- replaceBodyOfWhileWithReduction stmt
        decl <- createDeclToReducedExprs
        case decl of
          Just decl -> do
            modify $ \s -> s { compilerOutput = compilerOutput s ++ ["; Declaration(s) of variables which reduce multiplication in while loop:"] }
            compile decl
          Nothing -> return ()
        let condLabel = "while_cond_" ++ show counter
        let bodyLabel = "while_body_" ++ show counter
        let endLabel = "while_end_" ++ show counter
        case expr of
          Latte.Abs.ELitFalse _ -> return ()
          Latte.Abs.ELitTrue _ -> do
            case newBody of
              Just newBody -> do
                commonWhilePart p expr newBody condLabel bodyLabel bodyLabel counter True
              Nothing -> return ()
          _ -> do
            case newBody of
              Just newBody -> do
                commonWhilePart p expr newBody condLabel bodyLabel endLabel counter False
              Nothing -> return ()
        popInductionVariablesFrame
        popReduceStacks
      Latte.Abs.Incr p ident -> do
        removeExprsWithVarFromAllFrames (name ident)
        commonDecrIncrOperation ident "add"
      Latte.Abs.Decr p ident -> do
        removeExprsWithVarFromAllFrames (name ident)
        commonDecrIncrOperation ident "sub"
      Latte.Abs.Ret p expr -> do
        insideInlineFuncion <- checkIfCodeIsInsideInlineFunction
        if insideInlineFuncion then do
            topLabel <- getTopLabel
            labelsUsedInReturn <- gets labelsUsedInReturn
            let isLabelUsed = topLabel `elem` labelsUsedInReturn
            if isLabelUsed then do
              labelCounter <- getNextLabelCounterAndUpdate
              let brInstr = "br label %inline_return_" ++ show labelCounter
              let label = "inline_return_" ++ show labelCounter
              modify $ \s -> s { labelsUsedInReturn = label : labelsUsedInReturn }
              pushLabelToStack label
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [brInstr, label++ ":"] }
              e <- compilerExpr expr
              addInlineFunctionReturnToFrame label e
            else do
              modify $ \s -> s { labelsUsedInReturn = topLabel : labelsUsedInReturn }
              e <- compilerExpr expr
              addInlineFunctionReturnToFrame topLabel e
            endOfFunctionLabel <- getEndOfActualFunctionLabel
            modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endOfFunctionLabel], isBrLastStmt = True}
            modify $ \s -> s { returnReached = True }
            return ()
        else do
          e <- compilerExpr expr
          exprWithType <- combineTypeAndIndentOfExpr expr e
          let returnText = "ret" ++ " " ++ exprWithType
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [returnText],
                            returnReached = True }
      Latte.Abs.VRet p -> do
        itIsInsideInline <- checkIfCodeIsInsideInlineFunction
        if itIsInsideInline then do
          endOfFunctionLabel <- getEndOfActualFunctionLabel
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endOfFunctionLabel], isBrLastStmt = True}
          modify $ \s -> s { returnReached = True }
        else
         modify $ \s -> s { compilerOutput = compilerOutput s ++ ["ret void"],
                        returnReached = True }
      Latte.Abs.SExp _ expr -> do
        compilerExpr expr
        return ()

------------------------------------------------
------------------MAIN SECTION------------------
------------------------------------------------
runCompiler :: (Compile a) => a -> Map (Latte.Abs.Ident, [Type]) Type -> Map Latte.Abs.Expr Type ->
  Map (Latte.Abs.Ident, [Type]) (Type, [Latte.Abs.Arg], Latte.Abs.Block) -> Either String CompilerState
runCompiler program functionsSignatures exprTypes inlineFunctions = execStateT (compile program) initialState
  where
    initialState = CompilerState {
      compilerOutput = predefFunctions,
      variablesStack = [],
      variablesCounter = 0,
      labelCounter = 0,
      phiCounter = 0,
      functionsSignatures = functionsSignatures,
      exprTypes = exprTypes,
      inlineFunctions = inlineFunctions,
      inlineFunctionsReturnStack = [],
      inlineFunctionsToken = 0,
      inlineFunctionsLabelsStack = [],
      inlineReturnReached = False,
      currentInlineFunctionType = Void,
      labelsUsedInReturn = [],
      computedExprsStack = [],
      arguments = Map.empty,
      stringPool = Map.empty,
      returnReached = False,
      concatWasDeclared = False,
      phiNodesStack = [],
      labelsStack = [],
      inductionVariablesStack = [],
      tokenWhile = 0,
      isBrLastStmt = False,
      exprsToReduceStack = [],
      variablesWithReducedExprStack = [],
      reducedVariablesCounter = 0,
      declaredVariablesInIfElseBlock = [],
      tokenIfElse = 0,
      moreThanTwoTokensWhileIsUp = False
    }
    predefFunctions =
      [
        "declare void @printString(i8* %str)",
        "declare void @printInt(i32 %i)",
        "declare void @error()",
        "declare i32 @readInt()",
        "declare i8* @readString()"
      ]