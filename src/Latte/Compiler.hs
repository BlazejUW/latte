

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE BlockArguments #-}

module Latte.Compiler where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad (forM, forM_, unless, when, zipWithM)
import Control.Monad.State (StateT, execStateT, get, gets, modify, put)
import Data.Map (Map, adjust, delete, insert)
import qualified Data.Map as Map
import Data.Maybe (isJust, catMaybes)
import Data.List (intercalate)
import qualified Latte.Abs
import Latte.Helpers (Type (Boolean, Integer, String, Void), errLocation, functionName, keywordToType, name, typeToKeyword, typeToLlvmKeyword, convertToLlvmChar)
import Data.Void (Void)
import qualified Distribution.Simple as Latte


--STATE SECTION
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
    inlineFunctionsReturnStack :: [Map String String], -- (nazwa labelki, nazwa rejestru zwracającego wartość)
    inlineFunctionsToken :: Int,
    inlineFunctionsLabelsStack :: [String],
    returnRegisterInsideInlineFunction :: String,
    labelsUsedInReturn :: [String],
    computedExprsStack :: [[(Latte.Abs.Expr, String)]],
    arguments :: Map String Type,
    stringPool :: Map String Int,
    returnReached :: Bool,
    concatWasDeclared :: Bool,
    phiNodesStack :: [Map String (Type,String)],
    labelsStack :: [String],
    inductionVariablesStack :: [Map String Bool], -- (nazwa, typ, czy jest zmienną indukcyjną)
    tokenWhile :: Int
  }
  deriving (Eq, Ord, Show)

type LCS a = StateT CompilerState (Either CompilerError) a
type CompilerError = String
type CompilerOutput = [String]


--FUNCTIONS SECTION
getExpressionType :: Latte.Abs.Expr -> LCS Type
getExpressionType expr = do
  s <- get
  case Map.lookup expr (exprTypes s) of
    Just t -> return t
    Nothing -> throwError $ "Expression type not found: " ++ show expr

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

lookupVariable :: String -> LCS (Maybe (Type, String))
lookupVariable varName = gets $ \s ->
  let search [] = Nothing
      search (frame:rest) = case Map.lookup varName frame of
        Nothing -> search rest
        justVar -> justVar
  in search (variablesStack s)

isFunctionInline :: (Latte.Abs.Ident, [Type]) -> LCS Bool
isFunctionInline function = do
  gets (Map.member function . inlineFunctions)

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
      -- Istniejące ramki
      s { computedExprsStack = ((expr, llvmVarName) : currentFrame) : rest }
    [] ->
      -- Pusty stos, inicjalizuj z nową ramką
      s { computedExprsStack = [[(expr, llvmVarName)]] }

getVariableType :: String -> LCS Type
getVariableType identifier = do
  state <- get
  let findTypeInStack [] = error $ "Variable " ++ identifier ++ " not found"
      findTypeInStack (frame:rest) = case Map.lookup identifier frame of
        Just (varType, _) -> return varType
        Nothing -> findTypeInStack rest
  findTypeInStack (variablesStack state)

getNextVariableAndUpdate :: LCS Int
getNextVariableAndUpdate = do
  s <- get
  let counter = variablesCounter s
  modify $ \s -> s { variablesCounter = counter + 1 }
  return counter

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

convertToLlvmString :: String -> String
convertToLlvmString s = concatMap convertToLlvmChar s ++ "\\00"

combineTypeAndIndentOfExpr :: Latte.Abs.Expr -> String -> LCS String
combineTypeAndIndentOfExpr expr exprStr = do
  exprType <- getExpressionType expr
  return $ typeToLlvmKeyword exprType ++ " " ++ exprStr

commonDecrIncrOperation :: Latte.Abs.Ident -> String -> StateT CompilerState (Either CompilerError) ()
commonDecrIncrOperation ident op = do
  s <- get
  let varName = name ident
  maybeVar <- lookupVariable varName
  case maybeVar of
    Just (varType, llvmVarName) -> do
      opCounter <- getNextVariableAndUpdate
      let opInstr = "%" ++ show opCounter ++ " = " ++ op ++ " " ++ typeToLlvmKeyword varType ++ " %" ++ llvmVarName ++ ", 1"
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [opInstr] }
      updateVariableInStack varName varType (show opCounter)
      addPhiNodeToFrame varName varType (show opCounter)
    Nothing -> throwError $ "Variable not defined: " ++ varName

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

commonWhilePart :: Latte.Abs.Expr -> Latte.Abs.Stmt -> String -> String -> String -> Int -> Bool -> StateT CompilerState (Either CompilerError) ()
commonWhilePart expr stmt condLabel bodyLabel endLabel counter isAlwaysTrue = do
    tokenWhileUp
    pushPhiNodesFrame
    originalReturnFlag <- gets returnReached
    modify $ \s -> s { returnReached = False }
    pushExprsFrame
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ condLabel] }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [condLabel ++ ":"] }
    pushPhiNodesFrame
    fakeWhileRunAndAddPhiBlock expr stmt counter
    variablesStackAfterFakeLoop <- gets variablesStack
    popPhiNodesFrameAndMergeInIntoStack
    e <- compilerExpr expr
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ e ++ ", label %" ++ bodyLabel ++ ", label %" ++ endLabel] }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [bodyLabel ++ ":"] }
    pushLabelToStack bodyLabel
    compile stmt
    popExprsFrame
    popPhiNodesFrameAndMergeInIntoStack
    modify $ \s -> s { variablesStack = variablesStackAfterFakeLoop }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ condLabel] }
    modify $ \s -> s { returnReached = originalReturnFlag}
    tokenWhileDown
    if isAlwaysTrue then
      return ()
    else do
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
      pushLabelToStack endLabel

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

printArg :: Latte.Abs.Arg -> String
printArg (Latte.Abs.Arg _ argType ident) =
    typeToLlvmKeyword (keywordToType argType) ++ " %" ++ name ident

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
    return $ expr11EqExpr21 && expr12EqExpr22 && opEq
  (Latte.Abs.EMul _ expr11 op1 expr12, Latte.Abs.EMul _ expr21 op2 expr22) -> do
    expr11EqExpr21 <- isExprEqual expr11 expr21
    expr12EqExpr22 <- isExprEqual expr12 expr22
    let opEq = compareLatteMulOp op1 op2
    return $ expr11EqExpr21 && expr12EqExpr22 && opEq
  (Latte.Abs.EAnd _ expr11 expr12, Latte.Abs.EAnd _ expr21 expr22) -> do
    expr11EqExpr21 <- isExprEqual expr11 expr21
    expr12EqExpr22 <- isExprEqual expr12 expr22
    return $ expr11EqExpr21 && expr12EqExpr22
  (Latte.Abs.EOr _ expr11 expr12, Latte.Abs.EOr _ expr21 expr22) -> do
    expr11EqExpr21 <- isExprEqual expr11 expr21
    expr12EqExpr22 <- isExprEqual expr12 expr22
    return $ expr11EqExpr21 && expr12EqExpr22
  (Latte.Abs.Neg _ expr1, Latte.Abs.Neg _ expr2) -> isExprEqual expr1 expr2
  (Latte.Abs.Not _ expr1, Latte.Abs.Not _ expr2) -> isExprEqual expr1 expr2
  (Latte.Abs.ERel _ expr11 op1 expr12, Latte.Abs.ERel _ expr21 op2 expr22) -> do
    expr11EqExpr21 <- isExprEqual expr11 expr21
    expr12EqExpr22 <- isExprEqual expr12 expr22
    let opEq = compareLatteRelOp op1 op2
    return $ expr11EqExpr21 && expr12EqExpr22 && opEq
  _ -> return False

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

fakeInitInsteadOfAlloca :: String -> Type -> String -> Latte.Abs.Expr' a -> Bool -> StateT CompilerState (Either CompilerError) ()
fakeInitInsteadOfAlloca varName varType exprString expr isItAssignment = case expr of
  Latte.Abs.ELitInt _ _ -> do
    addCounter <- getNextVariableAndUpdate
    let addInstr = "%" ++ show addCounter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, " ++ exprString
    if isItAssignment then do
      updateVariableInStack varName varType (show addCounter)
      addPhiNodeToFrame varName varType (show addCounter)
    else do
      addVariableToFrame varName varType (show addCounter)
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
  Latte.Abs.ELitTrue _ -> do
    addCounter <- getNextVariableAndUpdate
    let addInstr = "%" ++ show addCounter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, 1"
    if isItAssignment then do
      updateVariableInStack varName varType (show addCounter)
      addPhiNodeToFrame varName varType (show addCounter)
    else do
      addVariableToFrame varName varType (show addCounter)
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
  Latte.Abs.ELitFalse _ -> do
    addCounter <- getNextVariableAndUpdate
    let addInstr = "%" ++ show addCounter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, 0"
    if isItAssignment then do
      updateVariableInStack varName varType (show addCounter)
      addPhiNodeToFrame varName varType (show addCounter)
    else do
      addVariableToFrame varName varType (show addCounter)
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
                addVariableToFrame varName varType (removeLeadingPercent exprString)
            _ -> do
              addCounter <- getNextVariableAndUpdate
              let addInstr = "%" ++ show addCounter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, %" ++ llvmVarName
              if isItAssignment then do
                updateVariableInStack varName varType (show addCounter)
                addPhiNodeToFrame varName varType (show addCounter)
              else do
                addVariableToFrame varName varType (show addCounter)
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
    else do
      if isItAssignment then do
        updateVariableInStack varName varType (removeLeadingPercent exprString)
        addPhiNodeToFrame varName varType (removeLeadingPercent exprString)
      else do
        addVariableToFrame varName varType (removeLeadingPercent exprString)
  _ -> do
    if isItAssignment then do
      updateVariableInStack varName varType (removeLeadingPercent exprString)
      addPhiNodeToFrame varName varType (removeLeadingPercent exprString)
    else do
      addVariableToFrame varName varType (removeLeadingPercent exprString)


removeLeadingPercent :: String -> String
removeLeadingPercent s = case s of
    ('%':rest) -> rest
    _ -> s

declareEmptyStringIfNotExists :: LCS String
declareEmptyStringIfNotExists = do
  s <- get
  case Map.lookup "" (stringPool s) of
    Just label -> return (show label)
    Nothing -> do
      stringId <- findOrDeclareString ""
      let strLabel = "@str" ++ show stringId
      let llvmString = convertToLlvmString ""
      let declaration = strLabel ++ " = private unnamed_addr constant [ 1 x i8] c\"" ++ llvmString ++ "\""
      modify $ \s -> s {compilerOutput = declaration : compilerOutput s}
      return strLabel

pushPhiNodesFrame :: LCS ()
pushPhiNodesFrame = modify $ \s -> s { phiNodesStack = Map.empty : phiNodesStack s }

popPhiNodesFrameAndMergeInIntoStack :: LCS ()
popPhiNodesFrameAndMergeInIntoStack = do
  topFrame <- gets $ \s -> case phiNodesStack s of
    (frame:_) -> frame
    _ -> Map.empty
  stack <- gets phiNodesStack
  case stack of
    -- Jeśli na stosie jest tylko jedna ramka, zaktualizuj stan stosu
    -- bez usuwania tej ramki
    [_] -> modify $ \s -> s { phiNodesStack = [topFrame] }
    -- Jeśli na stosie jest więcej niż jedna ramka
    (_:rest) -> do
      -- Usuń najwyższą ramkę ze stosu
      popPhiNodesFrame
      -- Pobierz aktualny stan stosu
      newStack <- gets phiNodesStack
      case newStack of
        (currentTopFrame:rest) -> do
          -- Połącz ramki i zaktualizuj stan stosu
          let mergedFrame = mergePhiFrames currentTopFrame topFrame
          modify $ \s -> s { phiNodesStack = mergedFrame : rest }
        -- Teoretycznie, ten przypadek nie powinien mieć miejsca,
        -- ponieważ obsługujemy już wcześniej przypadek pojedynczej ramki
        _ -> return ()
    -- Gdy stos jest pusty, dodaj topFrame
    [] -> modify $ \s -> s { phiNodesStack = [topFrame] }

popPhiNodesFrame :: LCS ()
popPhiNodesFrame = modify $ \s -> s {
  phiNodesStack = case phiNodesStack s of
      (_:rest) -> rest
      [] -> []
  }

mergePhiFrames :: Map String (Type, String) -> Map String (Type, String) -> Map String (Type, String)
mergePhiFrames frame1 frame2 = Map.union frame2 frame1  -- frame2 ma pierwszeństwo

getTopPhiFrame :: LCS (Map String (Type, String))
getTopPhiFrame = gets $ \s -> case phiNodesStack s of
  (topFrame:_) -> topFrame
  _ -> Map.empty

addPhiNodeToFrame :: String -> Type -> String -> LCS ()
addPhiNodeToFrame varName varType llvmVarName = modify $ \s ->
  let (currentFrame:rest) = phiNodesStack s
  in s { phiNodesStack = Map.insert varName (varType, llvmVarName) currentFrame : rest }

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
  s <- get
  labelCounter <- getNextPhiCounterAndUpdate
  updateVariableInStack varName varType ("phi_value_" ++ show labelCounter)
  updateVariableInPhiTopFrame varName varType ("phi_value_" ++ show labelCounter)
  return ("%" ++ "phi_value_" ++ show labelCounter ++ " = phi " ++
    typeToLlvmKeyword varType ++ " [ %" ++ regBefore ++ ", %" ++
    labelBefore ++ " ], [ %" ++ regAfter ++ ", %" ++ labelAfter ++ " ]")

updateVariableInPhiTopFrame :: String -> Type -> String -> LCS ()
updateVariableInPhiTopFrame  varName varType newReg = do
  s <- get
  let (currentFrame:rest) = phiNodesStack s
  let updatedFrame = Map.insert varName (varType, newReg) currentFrame
  put s { phiNodesStack = updatedFrame : rest }

fakeWhileRunAndAddPhiBlock :: Latte.Abs.Expr -> Latte.Abs.Stmt -> Int -> StateT CompilerState (Either CompilerError) ()
fakeWhileRunAndAddPhiBlock expr stmt labelCounter = do
  s <- get
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
    _ -> do
      return ()

handlePhiBlockAtIfElse :: Map String (Type, String) -> Map String (Type, String) -> String -> String -> LCS [String]
handlePhiBlockAtIfElse phiFrameAfterTrue phiFrameAfterFalse lastLabelInTrueBranch lastLabelInFalseBranch = do
  -- Krok 1: Obsługa elementów wspólnych dla obu gałęzi if-else
  commonElements <- forM (Map.toList phiFrameAfterTrue) $ \(varName, (varType, regAfterTrue)) -> do
    case Map.lookup varName phiFrameAfterFalse of
      Just (_, regAfterFalse) -> do
        -- Tworzenie phi-zmiennej dla elementów wspólnych
        phiNode <- createPhiNode varName varType regAfterTrue regAfterFalse lastLabelInTrueBranch lastLabelInFalseBranch
        return $ Just phiNode
      Nothing -> do
        -- W przypadku braku elementu w drugiej ramce
        regBefore <- getVariableNameFromStack varName
        phiNode <- createPhiNode varName varType regBefore regAfterTrue lastLabelInFalseBranch lastLabelInTrueBranch
        return $ Just phiNode

  -- Krok 2: Obsługa pozostałych elementów
  remainingElements <- forM (Map.toList phiFrameAfterFalse) $ \(varName, (varType, regAfterFalse)) -> do
    -- Sprawdzenie czy element nie został już przetworzony
    case Map.lookup varName phiFrameAfterTrue of
      Just _ -> return Nothing -- Element już przetworzony
      Nothing -> do
        regBefore <- getVariableNameFromStack varName
        phiNode <- createPhiNode varName varType regBefore regAfterFalse lastLabelInTrueBranch lastLabelInFalseBranch
        return $ Just phiNode

  -- Zwracanie wszystkich utworzonych węzłów phi
  return $ catMaybes (commonElements ++ remainingElements)

-- Funkcja pomocnicza do pobierania nazwy zmiennej ze stosu
getVariableNameFromStack :: String -> LCS String
getVariableNameFromStack varName = do
  maybeVar <- lookupVariable varName
  case maybeVar of
    Just (_, llvmVarName) -> return llvmVarName
    Nothing -> throwError $ "Variable not defined in stack: " ++ varName


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

pushInductionVariablesFrame :: LCS ()
pushInductionVariablesFrame = modify $ \s -> s { inductionVariablesStack = Map.empty : inductionVariablesStack s }

popInductionVariablesFrame :: LCS ()
popInductionVariablesFrame = modify $ \s -> s {
  inductionVariablesStack = case inductionVariablesStack s of
      (_:rest) -> rest
      [] -> []
  }

addInductionVariableToFrame :: String -> LCS ()
addInductionVariableToFrame varName = modify $ \s ->
  let (currentFrame:rest) = inductionVariablesStack s
  in s { inductionVariablesStack = Map.insert varName True currentFrame : rest }

markVariableAsNotInduction :: String -> LCS ()
markVariableAsNotInduction varName = modify $ \s ->
  let (currentFrame:rest) = inductionVariablesStack s
  in s { inductionVariablesStack = Map.insert varName False currentFrame : rest }

getTopInductionVariablesFrame :: LCS (Map String Bool)
getTopInductionVariablesFrame = gets $ \s -> case inductionVariablesStack s of
  (topFrame:_) -> topFrame
  _ -> Map.empty

lookupForInductionVariableInTopFrame :: String -> LCS (Maybe Bool)
lookupForInductionVariableInTopFrame varName = do
  topFrame <- getTopInductionVariablesFrame
  case Map.lookup varName topFrame of
    Just var -> return $ Just var
    Nothing -> return Nothing

analyzeStmtInLookingForIV :: Latte.Abs.Stmt -> LCS ()
analyzeStmtInLookingForIV stmt  = case stmt of
    Latte.Abs.BStmt _ block -> analyzeBlockInLookingForIV  block
    Latte.Abs.Ass _ ident expr -> analyzeAssignmentInLookingForIV  (name ident) expr
    Latte.Abs.Incr _ ident -> potentiallyMarkAsInductionVar (name ident)
    Latte.Abs.Decr _ ident -> potentiallyMarkAsInductionVar (name ident)
    Latte.Abs.Cond _ _ stms -> analyzeStmtInLookingForIV stmt
    Latte.Abs.CondElse _ _ stmt1 stmt2 -> do
      analyzeStmtInLookingForIV stmt1
      analyzeStmtInLookingForIV stmt2
    _ -> return ()  -- Pominięcie innych typów instrukcji

-- Analiza bloku instrukcji
analyzeBlockInLookingForIV  :: Latte.Abs.Block  -> LCS ()
analyzeBlockInLookingForIV  (Latte.Abs.Block _ stmts) = mapM_ analyzeStmtInLookingForIV  stmts

-- Analiza przypisania
analyzeAssignmentInLookingForIV  :: String -> Latte.Abs.Expr  -> LCS ()
analyzeAssignmentInLookingForIV  varName expr  = case expr of
    Latte.Abs.EAdd _ expr1 _ expr2 -> analyzeAddMulExpr varName expr1 expr2
    Latte.Abs.EMul _ expr1 _ expr2 -> analyzeAddMulExpr varName expr1 expr2
    Latte.Abs.ELitInt _ _ -> potentiallyMarkAsInductionVar varName
    Latte.Abs.EVar _ ident -> do
      isPotentiallyInduction <- analyzeExprForInduction varName expr
      if isPotentiallyInduction then
        potentiallyMarkAsInductionVar varName
      else do
        markVariableAsNotInduction varName
    Latte.Abs.Neg _ expr -> do
      isPotentiallyInduction <- analyzeExprForInduction varName expr
      if isPotentiallyInduction then
        potentiallyMarkAsInductionVar varName
      else do
        markVariableAsNotInduction varName
    _ -> markVariableAsNotInduction varName

-- Analiza dodawania i mnożenia
analyzeAddMulExpr :: String -> Latte.Abs.Expr -> Latte.Abs.Expr -> LCS ()
analyzeAddMulExpr varName expr1 expr2 = do
  results <- mapM (analyzeExprForInduction varName) [expr1, expr2]
  if and results then
    potentiallyMarkAsInductionVar varName
  else
    markVariableAsNotInduction varName

analyzeExprForInduction :: String -> Latte.Abs.Expr -> LCS Bool
analyzeExprForInduction varName expr = case expr of
    Latte.Abs.EVar _ ident -> do
        maybeIndVar <- lookupForInductionVariableInTopFrame (name ident)
        case maybeIndVar of
            Just True -> return True
            _ -> return False
    Latte.Abs.ELitInt _ _ -> return True
    Latte.Abs.Neg _ expr -> analyzeExprForInduction varName expr
    Latte.Abs.EAdd _ expr1 op expr2 -> do
      res1 <- analyzeExprForInduction varName expr1
      res2 <- analyzeExprForInduction varName expr2
      return (res1 && res2)
    Latte.Abs.EMul _ expr1 op expr2 -> do
      res1 <- analyzeExprForInduction varName expr1
      res2 <- analyzeExprForInduction varName expr2
      return (res1 && res2)
    _ -> return False

-- Oznaczanie zmiennej jako indukcyjnej
potentiallyMarkAsInductionVar :: String -> LCS ()
potentiallyMarkAsInductionVar varName = do
  lookupResult <- lookupForInductionVariableInTopFrame varName
  case lookupResult of
    Just _ -> return ()
    Nothing -> do
      addInductionVariableToFrame varName
      return ()

analyzeWhileLoop :: Latte.Abs.Stmt -> LCS () --funkcja odpowiadająca za znalezienie zmiennych indukcyjnych w pętli while
analyzeWhileLoop stmt = do
    pushInductionVariablesFrame
    iterativelyAnalyzeBodyOfWhile stmt
    popInductionVariablesFrame

iterativelyAnalyzeBodyOfWhile :: Latte.Abs.Stmt -> LCS ()
iterativelyAnalyzeBodyOfWhile stmt = do
    previousCount <- countInductionVariables
    analyzeStmtInLookingForIV stmt
    newCount <- countInductionVariables
    unless (newCount == previousCount) $
        iterativelyAnalyzeBodyOfWhile stmt

countInductionVariables :: LCS Int
countInductionVariables = do
    length . filter id . Map.elems <$> getTopInductionVariablesFrame

getArgumentRegisters :: Latte.Abs.Expr -> LCS String
getArgumentRegisters expr = do
  case expr of
    Latte.Abs.ELitInt _ val -> do
      counter <- getNextVariableAndUpdate
      let addInstr = "%" ++ show counter ++ " = add i32 0, %" ++ show val
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
      return ("%" ++ show counter)
    Latte.Abs.ELitTrue _ -> do
      counter <- getNextVariableAndUpdate
      let addInstr = "%" ++ show counter ++ " = add i32 0, 1"
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
      return ("%" ++ show counter)
    Latte.Abs.ELitFalse _ -> do
      counter <- getNextVariableAndUpdate
      let addInstr = "%" ++ show counter ++ " = add i32 0, 0"
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
      return ("%" ++ show counter)
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
  let label = "inline_function_" ++ show labelCouter
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
  modify $ \s -> s { returnRegisterInsideInlineFunction = "" }
  pushExprsFrame
  pushVariablesFrame
  pushInlineFunctionReturnFrame
  let functionName = name ident
  (retType,argsAbs,body) <- getInlineFunctionItems ident argsTypes
  addInlineFunctionArgumentsToFrame argsAbs argsRegisters argsTypes
  label <- findLabelForThisInlineFunction
  compile body
  pushLabelToStack label
  register <- handleReturnPhiBlockInInlineFunction retType label
  popExprsFrame
  popVariablesFrame
  popInlineFunctionReturnFrame
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

handleReturnPhiBlockInInlineFunction :: Type -> String -> LCS String
handleReturnPhiBlockInInlineFunction functionType labelAtTheEnd = do
  modify $ \s -> s { compilerOutput = compilerOutput s ++ [labelAtTheEnd ++ ":"] }
  counter <- getNextVariableAndUpdate
  topFrame <- gets $ \s -> case inlineFunctionsReturnStack s of
    (frame:_) -> frame
    _ -> Map.empty
  let phiEntries = map (\(label, reg) -> "[ " ++ reg ++ ", %" ++ label ++ " ]") $ Map.toList topFrame
  let phiString = intercalate ", " phiEntries
  modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show counter ++ " = phi "++ typeToLlvmKeyword functionType ++ " " ++ phiString] }
  return ("%" ++ show counter)

--DO USUNIĘCIA
putMapToStringList :: Map.Map String (Type, String) -> [String]
putMapToStringList m = [k ++ ":" ++ v | (k, (_, v)) <- Map.toList m]

--EXPRESSIONS SECTION
class CompileExpr a where
  compilerExpr :: a -> LCS String

instance CompileExpr Latte.Abs.Expr where
  compilerExpr  node = case node of
    Latte.Abs.ELitInt _ val -> return (show val)
    Latte.Abs.ELitTrue _ -> return "1"
    Latte.Abs.ELitFalse _ -> return "0"
    Latte.Abs.EString _ str -> do
      lookupExpr <- lookupExprsFrame node
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
          let call = "%"++ show nextIndirectVariable ++ " = getelementptr inbounds [" ++ show stringLength ++
                    " x i8], [" ++ show stringLength ++ " x i8]* " ++ strLabel ++ ", i32 0, i32 0"
          when isNew $ do
            let declaration = strLabel ++ " = private unnamed_addr constant [" ++ show stringLength ++ " x i8] c\"" ++ llvmString ++ "\""
            modify $ \s -> s {compilerOutput = declaration : compilerOutput s}
          modify $ \s -> s {compilerOutput = compilerOutput s ++ [call]}
          addExprToFrame node (show nextIndirectVariable)
          return $ "%" ++ show nextIndirectVariable
    Latte.Abs.EAdd p l op r -> do
      lookupExpr <- lookupExprsFrame node
      case lookupExpr of
        Just llvmVarName -> return $ "%" ++ llvmVarName
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
              let call = "%" ++ show counter ++ " = call i8* @doNotUseThatNameConcat(i8* " ++ l' ++ ", i8* " ++ r' ++ ")"
              concatWasDeclared <- gets concatWasDeclared
              unless concatWasDeclared $ do
                modify $ \s -> s { compilerOutput = "declare i8* @doNotUseThatNameConcat(i8*, i8*)" : compilerOutput s }
                modify $ \s -> s { concatWasDeclared = True }
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [call] }
              addExprToFrame node (show counter)
              return $ "%" ++ show counter
            (String, _) -> throwError ("Cannot subtract strings, only concatenation is allowed, at " ++ errLocation p)
            (Integer, _) -> do
              let op' = case op of
                    Latte.Abs.Plus _ -> "add"
                    Latte.Abs.Minus _ -> "sub"
              modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show counter ++ " = " ++ op' ++ " i32 " ++ l' ++ ", " ++ r']}
              addExprToFrame node (show counter)
              return $ "%" ++ show counter
            _ -> throwError $ "Cannot add two: " ++ show lType ++ " and " ++ show r ++ ", at " ++ errLocation p
    Latte.Abs.EMul p l op r -> do
      lookupExpr <- lookupExprsFrame node
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
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show counter ++ " = " ++ op' ++ " i32 " ++ l' ++ ", " ++ r']}
          addExprToFrame node (show counter)
          return $ "%" ++ show counter
    Latte.Abs.Neg p expr -> do
      lookupExpr <- lookupExprsFrame node
      case lookupExpr of
        Just llvmVarName -> return $ "%" ++ llvmVarName
        Nothing -> do
          e <- compilerExpr expr
          counter <- getNextVariableAndUpdate
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show counter ++ " = sub i32 0, " ++ e] }
          addExprToFrame node (show counter)
          return $ "%" ++ show counter
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
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show resultVar ++ " = phi i1 [ " ++ rExpr ++ ", %" ++ trueLabelToPhi ++ " ], [ 0, %" ++ falseLabelToPhi ++ " ]"] }
      addExprToFrame node (show resultVar)
      return $ "%" ++ show resultVar
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
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show resultVar ++ " = phi i1 [ 1, %" ++ trueLabelToPhi ++ " ], [ " ++ rExpr ++ ", %" ++ falseLabelToPhi ++ " ]"] }
      addExprToFrame node (show resultVar)
      return $ "%" ++ show resultVar
    Latte.Abs.Not p expr -> do
      lookupExpr <- lookupExprsFrame node
      case lookupExpr of
        Just llvmVarName -> return $ "%" ++ llvmVarName
        Nothing -> do
          e <- compilerExpr expr
          counter <- getNextVariableAndUpdate
          let notInstr = "%" ++ show counter ++ " = xor i1 " ++ e ++ ", 1"
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [notInstr] }
          addExprToFrame node (show counter)
          return $ "%" ++ show counter
    Latte.Abs.ERel p l op r -> do
      lookupExpr <- lookupExprsFrame node
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
              let relInstr = "%" ++ show counter ++ " = icmp " ++ relOp ++ " i32 " ++ lExpr ++ ", " ++ rExpr
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [relInstr] }
              addExprToFrame node (show counter)
              return $ "%" ++ show counter
            Boolean -> do
              let relInstr = "%" ++ show counter ++ " = icmp " ++ relOp ++ " i1 " ++ lExpr ++ ", " ++ rExpr
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [relInstr] }
              addExprToFrame node (show counter)
              return $ "%" ++ show counter
            String -> do
              nextCounter <- getNextVariableAndUpdate
              let callStrcmp = "%" ++ show counter ++ " = call i32 @strcmp(i8* " ++ lExpr ++ ", i8* " ++ rExpr ++ ")"
              let icmpResult = "%" ++ show nextCounter ++ " = icmp " ++ relOp ++ " i32 %" ++ show counter ++ ", 0"
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [callStrcmp, icmpResult] }
              modify $ \s -> s { compilerOutput = "declare i32 @strcmp(i8*, i8*)" : compilerOutput s }
              addExprToFrame node (show nextCounter)
              return $ "%" ++ show nextCounter
            _ -> throwError $ "Comparison not supported for types: " ++ show lType ++ " and " ++ show r ++ ", at " ++ errLocation p
    Latte.Abs.EVar p ident -> do
      s <- get
      let varName = name ident
      maybeVar <- lookupVariable varName
      case maybeVar of
        Just (varType, llvmVarName) -> do
          addExprToFrame node llvmVarName
          return $ "%" ++ llvmVarName
        Nothing -> throwError $ "Variable not defined: " ++ varName ++ ", at " ++ errLocation p
    Latte.Abs.EApp p ident exprs -> do
      lookupExpr <- lookupExprsFrame node
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
            modify $ \s -> s { compilerOutput = compilerOutput s ++ ["; Inline function call: " ++ functionName (ident, argTypes)] }
            modify $ \s -> s { compilerOutput = compilerOutput s ++ ["; Arguments: " ++ intercalate ", " argRegisters] }
            inlineFunctionCall ident argRegisters argTypes
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
                let callInstr = "%" ++ show counter ++ " = " ++ funCall
                modify $ \s -> s { compilerOutput = compilerOutput s ++ [callInstr]}
                addExprToFrame node (show counter)
                return $ "%" ++ show counter


--COMPILE SECTION
class Compile a where
  compile :: a -> LCS ()

instance Compile Latte.Abs.Program where
    compile (Latte.Abs.Program _ topdefs) = do
        forM_ topdefs compile

instance Compile Latte.Abs.TopDef where
  compile fndef@(Latte.Abs.FnDef p t ident args block) = do
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
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["}"], labelsStack = [] }
    popVariablesFrame
    popExprsFrame


instance Compile Latte.Abs.Block where
  compile (Latte.Abs.Block _ stmts) = do
    forM_ stmts $ \stmt -> do
      returnFlag <- gets returnReached
      unless returnFlag $ compile stmt

instance Compile Latte.Abs.Stmt where
  compile stmt = do
    returnFlag <- gets returnReached
    if returnFlag
      then return ()
      else case stmt of
      Latte.Abs.Empty _ -> return ()
      Latte.Abs.BStmt _ block -> do
        pushExprsFrame
        pushVariablesFrame
        modify $ \s -> s { returnReached = False }
        compile block
        popVariablesFrame
        popExprsFrame
      Latte.Abs.Decl p type_ items -> forM_ items $ \item -> case item of
        Latte.Abs.NoInit _ ident -> do
          let varName = name ident
          let varType = keywordToType type_
          counter <- getNextVariableAndUpdate
          case varType of
            String -> do
              emptyStringLabel <- declareEmptyStringIfNotExists
              let call = "%"++ show counter ++ " = getelementptr inbounds [1 x i8], [1 x i8]* " ++
                    emptyStringLabel ++ ", i32 0, i32 0"
              addVariableToFrame varName varType (show counter)
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [call] }
            _ -> do
              let declToZero = "%" ++ show counter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, 0"
              addVariableToFrame varName varType (show counter)
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [declToZero] }
        Latte.Abs.Init _ ident expr -> do
          let varName = name ident
          let varType = keywordToType type_
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
          Nothing -> throwError $ "Variable not defined: " ++ varName ++ ", at" ++ errLocation p
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
            popPhiNodesFrameAndMergeInIntoStack
            unless returnFlag $ modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
            modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
            lastLabel <- getTopLabel
            pushLabelToStack endLabel
            modify $ \s -> s { returnReached = originalReturnFlag}
            popExprsFrame
            case phiFrameAfterIf of
              (phiFrameAfter:_) -> do
                phiBlock <- createPhiBlock framesBeforeIf phiFrameAfter topLabel lastLabel
                modify $ \s -> s { compilerOutput = compilerOutput s ++ phiBlock }
              _ -> return ()
      Latte.Abs.CondElse p expr stmt1 stmt2 -> do
        e <- compilerExpr expr
        case expr of
          Latte.Abs.ELitTrue _ -> compile stmt1
          Latte.Abs.ELitFalse _ -> compile stmt2
          _ -> do
            counter <- getNextLabelCounterAndUpdate
            let trueLabel = "if_true_" ++ show counter
            let falseLabel = "if_false_" ++ show counter
            let endLabel = "if_end_" ++ show counter
            modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ e ++ ", label %" ++ trueLabel ++ ", label %" ++ falseLabel] }
            -- true branch
            pushExprsFrame
            pushPhiNodesFrame
            pushLabelToStack trueLabel
            originalReturnFlag <- gets returnReached
            variableStackBeforeTrueBranch <- gets variablesStack
            modify $ \s -> s { compilerOutput = compilerOutput s ++ [trueLabel ++ ":"] }
            modify $ \s -> s { returnReached = False }
            compile stmt1
            returnFlag1 <- gets returnReached
            popExprsFrame
            modify $ \s -> s { variablesStack = variableStackBeforeTrueBranch }
            topLabelAfterTrueBranch <- getTopLabel
            phiFrameAfterTrueBranch <- getTopPhiFrame
            popPhiNodesFrame
            unless returnFlag1 $ modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
            -- false branch
            variableStackBeforeFalseBranch <- gets variablesStack
            pushExprsFrame
            pushPhiNodesFrame
            pushLabelToStack falseLabel
            modify $ \s -> s { compilerOutput = compilerOutput s ++ [falseLabel ++ ":"] }
            modify $ \s -> s { returnReached = False }
            compile stmt2
            modify $ \s -> s { variablesStack = variableStackBeforeFalseBranch }
            returnFlag2 <- gets returnReached
            topLabelAfterFalseBranch <- getTopLabel
            phiFrameAfterFalseBranch <- getTopPhiFrame
            popExprsFrame
            -- end
            unless (returnFlag1 && returnFlag2) $ do
              modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
              phiBlock <- handlePhiBlockAtIfElse phiFrameAfterTrueBranch phiFrameAfterFalseBranch topLabelAfterTrueBranch topLabelAfterFalseBranch
              modify $ \s -> s { compilerOutput = compilerOutput s ++ phiBlock }
              pushLabelToStack endLabel
              modify $ \s -> s { returnReached = originalReturnFlag}
            popExprsFrame
      Latte.Abs.While p expr stmt -> do
        counter <- getNextLabelCounterAndUpdate
        let condLabel = "while_cond_" ++ show counter
        let bodyLabel = "while_body_" ++ show counter
        let endLabel = "while_end_" ++ show counter
        case expr of
          Latte.Abs.ELitFalse _ -> return ()
          Latte.Abs.ELitTrue _ -> commonWhilePart expr stmt condLabel bodyLabel bodyLabel counter True
          _ -> commonWhilePart expr stmt condLabel bodyLabel endLabel counter False
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
              let label = "inline_return_" ++ show labelCounter
              pushLabelToStack label
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [label++ ":"] }
              e <- compilerExpr expr
              addInlineFunctionReturnToFrame label e
            else do
              modify $ \s -> s { labelsUsedInReturn = topLabel : labelsUsedInReturn }
              e <- compilerExpr expr
              addInlineFunctionReturnToFrame topLabel e
            endOfFunctionLabel <- getEndOfActualFunctionLabel
            modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endOfFunctionLabel] }
            return ()
          else do
            e <- compilerExpr expr
            exprWithType <- combineTypeAndIndentOfExpr expr e
            let returnText = "ret" ++ " " ++ exprWithType
            modify $ \s -> s { compilerOutput = compilerOutput s ++ [returnText],
                              returnReached = True }
      Latte.Abs.VRet p -> do

         modify $ \s -> s { compilerOutput = compilerOutput s ++ ["ret void"],
                        returnReached = True }
      Latte.Abs.SExp _ expr -> do
        compilerExpr expr
        return ()


--MAIN SECTION
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
      returnRegisterInsideInlineFunction = "",
      labelsUsedInReturn = [],
      computedExprsStack = [],
      arguments = Map.empty,
      stringPool = Map.empty,
      returnReached = False,
      concatWasDeclared = False,
      phiNodesStack = [],
      labelsStack = [],
      inductionVariablesStack = [],
      tokenWhile = 0
    }
    predefFunctions =
      [
        "declare void @printString(i8* %str)",
        "declare void @printInt(i32 %i)",
        "declare void @error()",
        "declare i32 @readInt()",
        "declare i8* @readString()"
      ]