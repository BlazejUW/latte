{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE BlockArguments #-}

module Latte.Compiler where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.State (StateT, execStateT, get, gets, modify, put)
import Data.Map (Map, adjust, delete, insert)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.List (intercalate)
import qualified Latte.Abs
import Latte.Helpers (Type (Boolean, Integer, String, Void), errLocation, functionName, keywordToType, name, typeToKeyword, typeToLlvmKeyword, convertToLlvmChar)
import Data.Void (Void)
import qualified Distribution.Simple as Latte

data CompilerState = CompilerState
  {
    compilerOutput :: CompilerOutput,
    variablesStack :: [Map String (Type,String)],
    variablesCounter :: Int,
    labelCounter :: Int,
    functionsSignatures :: Map (Latte.Abs.Ident, [Type]) Type,
    exprTypes :: Map Latte.Abs.Expr Type,
    arguments :: Map String Type,
    stringPool :: Map String Int,
    returnReached :: Bool,
    concatWasDeclared :: Bool
  }
  deriving (Eq, Ord, Show)

type LCS a = StateT CompilerState (Either CompilerError) a
type CompilerError = String
type CompilerOutput = [String]

getExpressionType :: Latte.Abs.Expr -> LCS Type
getExpressionType expr = do
  s <- get
  case Map.lookup expr (exprTypes s) of
    Just t -> return t
    Nothing -> throwError $ "Expression type not found: " ++ show expr

pushFrame :: LCS ()
pushFrame = modify $ \s -> s { variablesStack = Map.empty : variablesStack s }

popFrame :: LCS ()
popFrame = modify $ \s -> s {
  variablesStack = case variablesStack s of
      (_:rest) -> rest 
      [] -> []
  }

addVariableToFrame :: String -> Type -> String -> LCS ()
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

class CompileExpr a where
  compilerExpr :: a -> LCS String

instance CompileExpr Latte.Abs.Expr where
  compilerExpr = \case
    Latte.Abs.ELitInt _ val -> return (show val)
    Latte.Abs.ELitTrue _ -> return "1"
    Latte.Abs.ELitFalse _ -> return "0"
    Latte.Abs.EString _ str -> do
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
      return $ "%" ++ show nextIndirectVariable
    Latte.Abs.EAdd p l op r -> do
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
          return $ "%" ++ show counter
        Integer -> do
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show counter ++ " = " ++ op' ++ " i32 " ++ l' ++ ", " ++ r']}
          return $ "%" ++ show counter
        _ -> throwError $ "Cannot add two: " ++ show lType
    Latte.Abs.EMul p l op r -> do
      l' <- compilerExpr l
      r' <- compilerExpr r
      let op' = case op of
            Latte.Abs.Times _ -> "mul"
            Latte.Abs.Div _ -> "sdiv"
            Latte.Abs.Mod _ -> "srem"
      counter <- getNextVariableAndUpdate
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show counter ++ " = " ++ op' ++ " i32 " ++ l' ++ ", " ++ r']}
      return $ "%" ++ show counter
    Latte.Abs.Neg p expr -> do
      e <- compilerExpr expr
      counter <- getNextVariableAndUpdate
      let negInstr = "%" ++ show counter ++ " = sub i32 0, " ++ e
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [negInstr] }
      return $ "%" ++ show counter
    Latte.Abs.EAnd p l r -> do
      indirectVar <- getNextVariableAndUpdate
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show indirectVar ++ " = alloca i1"] }
      lExpr <- compilerExpr l
      labelCounter <- getNextLabelCounterAndUpdate
      let trueLabel = "and_true_" ++ show labelCounter
      let falseLabel = "and_false_" ++ show labelCounter
      let endLabel = "and_end_" ++ show labelCounter
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ lExpr ++ ", label %" ++ trueLabel ++ ", label %" ++ falseLabel] }
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [trueLabel ++ ":"] }
      rExpr <- compilerExpr r
      let resultAssign = "store i1 " ++ rExpr ++ ", i1* %" ++ show indirectVar
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [resultAssign,"br label %" ++ endLabel, falseLabel ++ ":"] }
      let resultAssign = "store i1 0, i1* %" ++ show indirectVar
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [resultAssign, "br label %" ++ endLabel, endLabel ++ ":"] }
      resultVar <- getNextVariableAndUpdate
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show resultVar ++ " = load i1, i1* %" ++ show indirectVar] }
      return $ "%" ++ show resultVar
    Latte.Abs.EOr p l r -> do
      indirectVar <- getNextVariableAndUpdate
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show indirectVar ++ " = alloca i1"] }
      lExpr <- compilerExpr l
      labelCounter <- getNextLabelCounterAndUpdate
      let falseLabel = "or_false_" ++ show labelCounter
      let trueLabel = "or_true_" ++ show labelCounter
      let endLabel = "or_end_" ++ show labelCounter
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ lExpr ++ ", label %" ++ trueLabel ++ ", label %" ++ falseLabel] }
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [trueLabel ++ ":"] }
      let interTrue = "store i1 1, i1* %" ++ show indirectVar
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [interTrue, "br label %" ++ endLabel, falseLabel ++ ":"] }
      rExpr <- compilerExpr r
      let interFalse = "store i1 " ++ rExpr ++ ", i1* %" ++ show indirectVar
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [interFalse, "br label %" ++ endLabel, endLabel ++ ":"] }
      resultVar <- getNextVariableAndUpdate
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["%" ++ show resultVar ++ " = load i1, i1* %" ++ show indirectVar] }
      return $ "%" ++ show resultVar
    Latte.Abs.Not p expr -> do
      e <- compilerExpr expr
      counter <- getNextVariableAndUpdate
      let notInstr = "%" ++ show counter ++ " = xor i1 " ++ e ++ ", 1"
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [notInstr] }
      return $ "%" ++ show counter
    Latte.Abs.ERel p l op r -> do
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
          return $ "%" ++ show counter
        Boolean -> do
          let relInstr = "%" ++ show counter ++ " = icmp " ++ relOp ++ " i1 " ++ lExpr ++ ", " ++ rExpr
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [relInstr] }
          return $ "%" ++ show counter
        String -> do
          nextCounter <- getNextVariableAndUpdate
          let callStrcmp = "%" ++ show counter ++ " = call i32 @strcmp(i8* " ++ lExpr ++ ", i8* " ++ rExpr ++ ")"
          let icmpResult = "%" ++ show nextCounter ++ " = icmp " ++ relOp ++ " i32 %" ++ show counter ++ ", 0"
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [callStrcmp, icmpResult] }
          modify $ \s -> s { compilerOutput = "declare i32 @strcmp(i8*, i8*)" : compilerOutput s }
          return $ "%" ++ show nextCounter
        _ -> throwError $ "Comparison not supported for types: " ++ show lType
    Latte.Abs.EVar p ident -> do
      s <- get
      let varName = name ident
      maybeVar <- lookupVariable varName
      case maybeVar of
        Just (varType, llvmVarName) -> do
          if Map.member varName (arguments s) && llvmVarName == varName then 
            return $ "%" ++ varName
          else do 
            counter <- getNextVariableAndUpdate
            let loadInstr = "%" ++ show counter ++ " = load " ++ typeToLlvmKeyword varType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ llvmVarName
            modify $ \s -> s { compilerOutput = compilerOutput s ++ [loadInstr]}
            return $ "%" ++ show counter
        Nothing -> throwError $ "Variable not defined: " ++ varName
    Latte.Abs.EApp p ident exprs -> do
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
          return $ "%" ++ show counter

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
  let counter = variablesCounter s + 1
  modify $ \s -> s { variablesCounter = counter }
  return counter

getNextLabelCounterAndUpdate :: LCS Int
getNextLabelCounterAndUpdate = do
  s <- get
  let counter =  labelCounter s + 1
  modify $ \s -> s { labelCounter = counter }
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

class Compile a where
  compile :: a -> LCS ()


instance Compile Latte.Abs.Program where
    compile (Latte.Abs.Program _ topdefs) = do
        forM_ topdefs compile


printArg :: Latte.Abs.Arg -> String
printArg (Latte.Abs.Arg _ argType ident) =
    typeToLlvmKeyword (keywordToType argType) ++ " %" ++ name ident


instance Compile Latte.Abs.TopDef where
  compile fndef@(Latte.Abs.FnDef p t ident args block) = do
    pushFrame
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
      compilerOutput = compilerOutput s ++ [funHeader, "{"], 
      variablesCounter = 0, 
      returnReached = False 
    }
    variablesCounter <- gets variablesCounter
    compile block
    returnFlag <- gets returnReached
    --if there is no return statement in function and type is void, add return void
    when (not returnFlag && retType == Void) $ do
      modify $ \s -> s { compilerOutput = compilerOutput s ++ ["ret void"] }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["}"] }
    popFrame



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
        pushFrame
        modify $ \s -> s { returnReached = False }
        compile block
        popFrame
      Latte.Abs.Decl p type_ items -> forM_ items $ \item -> case item of
        Latte.Abs.NoInit _ ident -> do
          let varName = name ident
          let varType = keywordToType type_
          counter <- getNextVariableAndUpdate
          addVariableToFrame varName varType (show counter)
          let varDeclaration = "%" ++ show counter ++ " = alloca " ++ typeToLlvmKeyword varType
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [varDeclaration] }
        Latte.Abs.Init _ ident expr -> do
          let varName = name ident
          let varType = keywordToType type_
          e <- compilerExpr expr
          counter <- getNextVariableAndUpdate
          addVariableToFrame varName varType (show counter)
          let varDeclaration = "%" ++ show counter ++ " = alloca " ++ typeToLlvmKeyword varType
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [varDeclaration] }
          exprWithType <- combineTypeAndIndentOfExpr expr e
          let varAssignment = "store " ++ exprWithType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ show counter
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [varAssignment]}
      Latte.Abs.Ass p ident expr -> do
        s <- get
        let varName = name ident
        maybeVar <- lookupVariable varName
        case maybeVar of
          Just (varType, llvmVarName) -> do
            e <- compilerExpr expr
            exprWithType <- combineTypeAndIndentOfExpr expr e
            if Map.member varName (arguments s) && (varName == llvmVarName) then do
              counter <- getNextVariableAndUpdate
              let allocaInstr = "%" ++ show counter ++ " = alloca " ++ typeToLlvmKeyword varType
              let storeInstr = "store " ++ exprWithType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ show counter
              addVariableToFrame varName varType (show counter)
              modify $ \s -> s {
                compilerOutput = compilerOutput s ++ [allocaInstr, storeInstr]
              } 
            else do
              let storeInstr = "store " ++ exprWithType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ llvmVarName
              modify $ \s -> s {compilerOutput = compilerOutput s ++ [storeInstr]}
          Nothing -> throwError $ "Variable not defined: " ++ varName
      Latte.Abs.Cond p expr stmt -> do
        e <- compilerExpr expr
        counter <- getNextLabelCounterAndUpdate
        let trueLabel = "if_true_" ++ show counter
        let endLabel = "if_end_" ++ show counter
        modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ e ++ ", label %" ++ trueLabel ++ ", label %" ++ endLabel] }
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [trueLabel ++ ":"] }
        originalReturnFlag <- gets returnReached
        modify $ \s -> s { returnReached = False }
        compile stmt
        returnFlag <- gets returnReached
        unless returnFlag $ do
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
        modify $ \s -> s { returnReached = originalReturnFlag}
      Latte.Abs.CondElse p expr stmt1 stmt2 -> do
        e <- compilerExpr expr
        counter <- getNextLabelCounterAndUpdate
        let trueLabel = "if_true_" ++ show counter
        let falseLabel = "if_false_" ++ show counter
        let endLabel = "if_end_" ++ show counter
        modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ e ++ ", label %" ++ trueLabel ++ ", label %" ++ falseLabel] }
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [trueLabel ++ ":"] }
        originalReturnFlag <- gets returnReached
        modify $ \s -> s { returnReached = False }
        compile stmt1
        returnFlag1 <- gets returnReached
        unless returnFlag1 $ do
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [falseLabel ++ ":"] }
        modify $ \s -> s { returnReached = False }
        compile stmt2
        returnFlag2 <- gets returnReached
        unless (returnFlag1 && returnFlag2) $ do
          modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ endLabel] }
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
          modify $ \s -> s { returnReached = originalReturnFlag}
      Latte.Abs.While p expr stmt -> do
        counter <- getNextLabelCounterAndUpdate
        let condLabel = "while_cond_" ++ show counter
        let bodyLabel = "while_body_" ++ show counter
        let endLabel = "while_end_" ++ show counter
        originalReturnFlag <- gets returnReached
        modify $ \s -> s { returnReached = False }
        modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ condLabel] }
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [condLabel ++ ":"] }
        e <- compilerExpr expr
        modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br i1 " ++ e ++ ", label %" ++ bodyLabel ++ ", label %" ++ endLabel] }
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [bodyLabel ++ ":"] }
        compile stmt
        modify $ \s -> s { compilerOutput = compilerOutput s ++ ["br label %" ++ condLabel] }
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [endLabel ++ ":"] }
        modify $ \s -> s { returnReached = originalReturnFlag }
      Latte.Abs.Incr p ident -> do
        commonDecrIncrOperation ident "add"
      Latte.Abs.Decr p ident -> do
        commonDecrIncrOperation ident "sub"
      Latte.Abs.Ret p expr -> do
        e <- compilerExpr expr
        exprWithType <- combineTypeAndIndentOfExpr expr e
        let returnText = "ret" ++ " " ++ exprWithType
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [returnText],
                          returnReached = True }
      Latte.Abs.VRet p -> do
        modify $ \s -> s { compilerOutput = compilerOutput s ++ ["ret void"],
                          returnReached = True }
      Latte.Abs.SExp _ expr -> do
        e  <- compilerExpr expr
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [e] }
      -- other -> throwError $ "Not implemented: " ++ show other

commonDecrIncrOperation ident op = do
  s <- get
  let varName = name ident
  maybeVar <- lookupVariable varName
  case maybeVar of
    Just (varType, llvmVarName) ->
      if Map.member varName (arguments s) && (varName == llvmVarName) then do
        opCounter <- getNextVariableAndUpdate
        let opInstr = "%" ++ show opCounter ++ " = " ++ op ++ " " ++ typeToLlvmKeyword varType ++ " %" ++ llvmVarName ++ ", 1"
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [opInstr] }
      else do
        loadCounter <- getNextVariableAndUpdate
        let loadInstr = "%" ++ show loadCounter ++ " = load " ++ typeToLlvmKeyword varType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ llvmVarName
        opCounter <- getNextVariableAndUpdate
        let opInstr = "%" ++ show opCounter ++ " = " ++ op ++ " " ++ typeToLlvmKeyword varType ++ " %" ++ show loadCounter ++ ", 1"
        let storeInstr = "store " ++ typeToLlvmKeyword varType ++ " %" ++ show opCounter ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ llvmVarName
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [loadInstr, opInstr, storeInstr] }
    Nothing -> throwError $ "Variable not defined: " ++ varName



runCompiler :: (Compile a) => a -> Map (Latte.Abs.Ident, [Type]) Type -> Map Latte.Abs.Expr Type -> Either String CompilerState
runCompiler program functionsSignatures exprTypes = execStateT (compile program) initialState
  where
    initialState = CompilerState {
      compilerOutput = predefFunctions,
      variablesStack = [Map.empty],
      variablesCounter = 0,
      labelCounter = 0,
      functionsSignatures = functionsSignatures,
      exprTypes = exprTypes,
      arguments = Map.empty,
      stringPool = Map.empty,
      returnReached = False,
      concatWasDeclared = False
    }

    predefFunctions =
      [
        "declare void @printString(i8* %str)",
        "declare void @printInt(i32 %i)",
        "declare void @error()",
        "declare i32 @readInt()",
        "declare i8* @readString()"
      ]