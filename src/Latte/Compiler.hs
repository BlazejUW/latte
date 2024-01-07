

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
    computedExprsStack :: [[(Latte.Abs.Expr, String)]],
    arguments :: Map String Type,
    stringPool :: Map String Int,
    returnReached :: Bool,
    concatWasDeclared :: Bool,
    phiNodesStack :: [Map String (Type,String)],
    labelsStack :: [String]
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

commonWhilePart :: Latte.Abs.Expr -> Latte.Abs.Stmt -> String -> String -> String -> Int -> Bool -> StateT CompilerState (Either CompilerError) ()
commonWhilePart expr stmt condLabel bodyLabel endLabel counter isAlwaysTrue = do
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


printArg :: Latte.Abs.Arg -> String
printArg (Latte.Abs.Arg _ argType ident) =
    typeToLlvmKeyword (keywordToType argType) ++ " %" ++ name ident

isEqual :: Latte.Abs.Expr -> Latte.Abs.Expr -> LCS Bool
isEqual expr1 expr2 = case (expr1, expr2) of
  (Latte.Abs.ELitTrue _, Latte.Abs.ELitTrue _) -> return True
  (Latte.Abs.ELitFalse _, Latte.Abs.ELitFalse _) -> return True
  (Latte.Abs.ELitInt _ val1, Latte.Abs.ELitInt _ val2) -> return $ val1 == val2
  (Latte.Abs.EString _ str1, Latte.Abs.EString _ str2) -> return $ str1 == str2
  (Latte.Abs.EVar _ ident1, Latte.Abs.EVar _ ident2) -> return $ name ident1 == name ident2
  (Latte.Abs.EAdd _ expr11 op1 expr12, Latte.Abs.EAdd _ expr21 op2 expr22) -> do
    expr11EqExpr21 <- isEqual expr11 expr21
    expr12EqExpr22 <- isEqual expr12 expr22
    return $ expr11EqExpr21 && expr12EqExpr22 && op1 == op2
  (Latte.Abs.EMul _ expr11 op1 expr12, Latte.Abs.EMul _ expr21 op2 expr22) -> do
    expr11EqExpr21 <- isEqual expr11 expr21
    expr12EqExpr22 <- isEqual expr12 expr22
    return $ expr11EqExpr21 && expr12EqExpr22 && op1 == op2
  (Latte.Abs.EAnd _ expr11 expr12, Latte.Abs.EAnd _ expr21 expr22) -> do
    expr11EqExpr21 <- isEqual expr11 expr21
    expr12EqExpr22 <- isEqual expr12 expr22
    return $ expr11EqExpr21 && expr12EqExpr22
  (Latte.Abs.EOr _ expr11 expr12, Latte.Abs.EOr _ expr21 expr22) -> do
    expr11EqExpr21 <- isEqual expr11 expr21
    expr12EqExpr22 <- isEqual expr12 expr22
    return $ expr11EqExpr21 && expr12EqExpr22
  (Latte.Abs.Neg _ expr1, Latte.Abs.Neg _ expr2) -> isEqual expr1 expr2
  (Latte.Abs.Not _ expr1, Latte.Abs.Not _ expr2) -> isEqual expr1 expr2
  (Latte.Abs.ERel _ expr11 op1 expr12, Latte.Abs.ERel _ expr21 op2 expr22) -> do
    expr11EqExpr21 <- isEqual expr11 expr21
    expr12EqExpr22 <- isEqual expr12 expr22
    return $ expr11EqExpr21 && expr12EqExpr22 && op1 == op2
  _ -> return False

lookupExprsFrame :: Latte.Abs.Expr -> LCS (Maybe String)
lookupExprsFrame expr = do
  frames <- gets computedExprsStack
  topFrame <- case frames of
    (topFrame:_) -> return topFrame
    _ -> return []
  searchExprsFrame topFrame expr


searchExprsFrame :: [(Latte.Abs.Expr, String)]-> Latte.Abs.Expr  -> LCS (Maybe String)
searchExprsFrame [] _ = return Nothing
searchExprsFrame ((expr, llvmVarName):rest) exprToSearch = do
  exprsEqual <- isEqual expr exprToSearch
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
      -- addPhiNodeToFrame varName varType (show addCounter)
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
  Latte.Abs.ELitTrue _ -> do
    addCounter <- getNextVariableAndUpdate
    let addInstr = "%" ++ show addCounter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, 1"
    if isItAssignment then do
      updateVariableInStack varName varType (show addCounter)
      addPhiNodeToFrame varName varType (show addCounter)
    else do
      addVariableToFrame varName varType (show addCounter)
      -- addPhiNodeToFrame varName varType (show addCounter)
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
  Latte.Abs.ELitFalse _ -> do
    addCounter <- getNextVariableAndUpdate
    let addInstr = "%" ++ show addCounter ++ " = add " ++ typeToLlvmKeyword varType ++ " 0, 0"
    if isItAssignment then do
      updateVariableInStack varName varType (show addCounter)
      addPhiNodeToFrame varName varType (show addCounter)
    else do
      addVariableToFrame varName varType (show addCounter)
      -- addPhiNodeToFrame varName varType (show addCounter)
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [addInstr] }
  _ -> do
    if isItAssignment then do
      updateVariableInStack varName varType (removeLeadingPercent exprString)
      addPhiNodeToFrame varName varType (removeLeadingPercent exprString)
    else do
      addVariableToFrame varName varType (removeLeadingPercent exprString)
      -- addPhiNodeToFrame varName varType (removeLeadingPercent exprString)


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
      let strLabel = "@empty_str" ++ show stringId
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
          stringId <- findOrDeclareString str
          let strLabel = "@str" ++ show stringId
          let llvmString = convertToLlvmString str
          let stringLength = length str + 1
          nextIndirectVariable <- getNextVariableAndUpdate
          let declaration = strLabel ++ " = private unnamed_addr constant [" ++ show stringLength ++ " x i8] c\"" ++ llvmString ++ "\""
          let call = "%"++ show nextIndirectVariable ++ " = getelementptr inbounds [" ++ show stringLength ++
                      " x i8], [" ++ show stringLength ++ " x i8]* " ++ strLabel ++ ", i32 0, i32 0"
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
          case lType of
            String -> do
              let call = "%" ++ show counter ++ " = call i8* @doNotUseThatNameConcat(i8* " ++ l' ++ ", i8* " ++ r' ++ ")"
              concatWasDeclared <- gets concatWasDeclared
              unless concatWasDeclared $ do
                modify $ \s -> s { compilerOutput = "declare i8* @doNotUseThatNameConcat(i8*, i8*)" : compilerOutput s }
                modify $ \s -> s { concatWasDeclared = True }
              modify $ \s -> s { compilerOutput = compilerOutput s ++ [call] }
              addExprToFrame node (show counter)
              return $ "%" ++ show counter
            Integer -> do
              modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show counter ++ " = " ++ op' ++ " i32 " ++ l' ++ ", " ++ r']}
              addExprToFrame node (show counter)
              return $ "%" ++ show counter
            _ -> throwError $ "Cannot add two: " ++ show lType
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
            _ -> throwError $ "Comparison not supported for types: " ++ show lType
    Latte.Abs.EVar p ident -> do
      s <- get
      let varName = name ident
      maybeVar <- lookupVariable varName
      case maybeVar of
        Just (varType, llvmVarName) -> do
          addExprToFrame node llvmVarName
          return $ "%" ++ llvmVarName
        Nothing -> throwError $ "Variable not defined: " ++ varName
    Latte.Abs.EApp p ident exprs -> do
      lookupExpr <- lookupExprsFrame node
      case lookupExpr of
        Just llvmVarName -> return $ "%" ++ llvmVarName
        Nothing -> do
          argExprs <- mapM compilerExpr exprs
          s <- get
          argTypes <- mapM getExpressionType exprs
          let funType = case Map.lookup (ident, argTypes) (functionsSignatures s) of
                Just t -> t
                _ -> error $ "Function " ++ functionName (ident, argTypes) ++ " not found"
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
            exprWithType <- combineTypeAndIndentOfExpr expr e
            fakeInitInsteadOfAlloca varName varType e expr True
          Nothing -> throwError $ "Variable not defined: " ++ varName
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
            unless returnFlag $ modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] } --TODO po co to
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
        e <- compilerExpr expr
        exprWithType <- combineTypeAndIndentOfExpr expr e
        let returnText = "ret" ++ " " ++ exprWithType
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [returnText],
                          returnReached = True }
      Latte.Abs.VRet p -> modify $ \s -> s { compilerOutput = compilerOutput s ++ ["ret void"],
                        returnReached = True }
      Latte.Abs.SExp _ expr -> do
        compilerExpr expr
        return ()


--MAIN SECTION
runCompiler :: (Compile a) => a -> Map (Latte.Abs.Ident, [Type]) Type -> Map Latte.Abs.Expr Type -> Either String CompilerState
runCompiler program functionsSignatures exprTypes = execStateT (compile program) initialState
  where
    initialState = CompilerState {
      compilerOutput = predefFunctions,
      variablesStack = [],
      variablesCounter = 0,
      labelCounter = 0,
      phiCounter = 0,
      functionsSignatures = functionsSignatures,
      exprTypes = exprTypes,
      computedExprsStack = [],
      arguments = Map.empty,
      stringPool = Map.empty,
      returnReached = False,
      concatWasDeclared = False,
      phiNodesStack = [],
      labelsStack = []
    }
    predefFunctions =
      [
        "declare void @printString(i8* %str)",
        "declare void @printInt(i32 %i)",
        "declare void @error()",
        "declare i32 @readInt()",
        "declare i8* @readString()"
      ]