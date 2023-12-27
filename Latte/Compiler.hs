{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Latte.Compiler where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.State (StateT, execStateT, get, gets, modify, put)
import Data.Map (Map, adjust, delete, insert)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.List (intercalate)
import qualified Latte.Abs
import Latte.Helpers (Type (Boolean, Integer, String, Void), errLocation, functionName, keywordToType, name, typeToKeyword, typeToLlvmKeyword)
import Data.Void (Void)
import qualified Distribution.Simple as Latte
-- import Latte.Typechecker (TypecheckerState)

data CompilerState = CompilerState
  {
    compilerOutput :: CompilerOutput,
    compilerVariables :: Map String Type,
    indirectVariablesCounter :: Int,
    functionsSignatures :: Map (Latte.Abs.Ident, [Type]) Type,
    exprTypes :: Map Latte.Abs.Expr Type,
    functionToDeclare :: Map (Latte.Abs.Ident, [Type]) Type,
    arguments :: Map String Type
    -- expectedReturnType :: Maybe Type
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

class CompileExpr a where
  compilerExpr :: a -> LCS String

instance CompileExpr Latte.Abs.Expr where
  compilerExpr = \case
    Latte.Abs.ELitInt _ val -> return (show val)
    Latte.Abs.ELitTrue _ -> return "1"
    Latte.Abs.ELitFalse _ -> return "0"
--     Latte.Abs.EString _ _ -> ""
    Latte.Abs.EAdd p l op r -> do
      l' <- compilerExpr l
      r' <- compilerExpr r
      let op' = case op of
            Latte.Abs.Plus _ -> "add"
            Latte.Abs.Minus _ -> "sub"
      return $ op' ++ " i32 " ++ l' ++ ", " ++ r'

--     Latte.Abs.EMul p l op r -> ""
--     Latte.Abs.Neg p expr -> ""
--     Latte.Abs.EAnd p l r -> ""
--     Latte.Abs.EOr p l r -> ""
--     Latte.Abs.Not p expr -> ""
--     Latte.Abs.ERel p l op r -> ""
    Latte.Abs.EVar p ident -> do
      s <- get
      let varName = name ident
      case Map.lookup varName (compilerVariables s) of
        Just varType -> do
          counter <- getNextIndirectVariableAndUpdate
          let loadInstr = if Map.member varName (arguments s)
                          then "%" ++ show counter ++ " = " ++ typeToLlvmKeyword varType ++ " %" ++ varName -- Obsługa argumentu
                          else "%" ++ show counter ++ " = load " ++ typeToLlvmKeyword varType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varName -- Obsługa zmiennej lokalnej
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
          counter <- getNextIndirectVariableAndUpdate
          let callInstr = "%" ++ show counter ++ " = " ++ funCall
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [callInstr]}
          return $ "%" ++ show counter
      -- jeśli return type to void to nie zwracamy nic, w przeciwnym wypadku zwracamy kolejny counter
      

getVariableType :: String -> LCS Type
getVariableType identifier = do
  state <- get
  let variables = compilerVariables state
  let variableType = case Map.lookup identifier variables of
                  Just t -> t
                  _ -> error $ "Variable " ++ identifier ++ " not found"
  return variableType

getNextIndirectVariableAndUpdate :: LCS Int
getNextIndirectVariableAndUpdate = do
  s <- get
  let counter = indirectVariablesCounter s + 1
  modify $ \s -> s { indirectVariablesCounter = counter }
  return counter

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
    let retType = keywordToType t
    let argsStr = intercalate ", " (map printArg args)
    --add args to variables
    forM_ args $ \(Latte.Abs.Arg _ argType ident) -> do
      let varName = name ident
      let varType = keywordToType argType
      modify $ \s -> s { compilerVariables = Map.insert varName varType (compilerVariables s),
                         arguments = Map.insert varName varType (arguments s)}
    let funName = name ident
    let funSignature =  typeToLlvmKeyword retType ++ " @" ++ funName ++ "(" ++ argsStr ++ ")"
    let funHeader = "define " ++ funSignature
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [funHeader] }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["{"] }
    -- get indirect variables counter and save it and next before block set it to 0
    indirectVariablesCounter <- gets indirectVariablesCounter
    modify $ \s -> s { indirectVariablesCounter = 0 }
    compile block
    -- restore indirect variables counter
    modify $ \s -> s { indirectVariablesCounter = indirectVariablesCounter }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["}"] }
    --delete args from variables
    forM_ args $ \(Latte.Abs.Arg _ _ ident) -> do
      let varName = name ident
      modify $ \s -> s { compilerVariables = Map.delete varName (compilerVariables s),
                         arguments = Map.delete varName (arguments s)}



instance Compile Latte.Abs.Block where
    compile (Latte.Abs.Block _ stmts) = do
        forM_ stmts compile


instance Compile Latte.Abs.Stmt where
  compile = \case
    Latte.Abs.Empty _ -> return ()
    Latte.Abs.BStmt _ block -> compile block
    Latte.Abs.Decl p type_ items -> forM_ items $ \item -> case item of
      Latte.Abs.NoInit _ ident -> do
        let varName = name ident
        let varType = keywordToType type_
        modify $ \s -> s { compilerVariables = Map.insert varName varType (compilerVariables s)}
        let varDeclaration = "%" ++ varName ++ " = alloca " ++ typeToLlvmKeyword varType
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [varDeclaration] }
      Latte.Abs.Init _ ident expr -> do
        let varName = name ident
        let varType = keywordToType type_
        modify $ \s -> s { compilerVariables = Map.insert varName varType (compilerVariables s)}
        let varDeclaration = "%" ++ varName ++ " = alloca " ++ typeToLlvmKeyword varType
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [varDeclaration] }
        e <- compilerExpr expr
        exprWithType <- combineTypeAndIndentOfExpr expr e
        let varAssignment = "store " ++ exprWithType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varName
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [varAssignment]}
    Latte.Abs.Ass p ident expr -> do
      s <- get
      let varName = name ident
      case Map.lookup varName (compilerVariables s) of
        Just varType -> do
          e <- compilerExpr expr
          exprWithType <- combineTypeAndIndentOfExpr expr e
          if Map.member varName (arguments s) then do
            counter <- getNextIndirectVariableAndUpdate
            let allocaInstr = "%" ++ show counter ++ " = alloca " ++ typeToLlvmKeyword varType
            let storeInstr = "store " ++ exprWithType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ show counter
            modify $ \s -> s {
              compilerOutput = compilerOutput s ++ [allocaInstr, storeInstr]
            }
          else do
            let storeInstr = "store " ++ exprWithType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varName
            modify $ \s -> s {
              compilerOutput = compilerOutput s ++ [storeInstr]
            }
        Nothing -> throwError $ "Variable not defined: " ++ varName
    -- Latte.Abs.Cond p expr stmt -> ""
    -- Latte.Abs.CondElse p expr stmt1 stmt2 ->    ""
    -- Latte.Abs.While p expr stmt ->     ""
    Latte.Abs.Incr p ident -> do
      commonDecrIncrOperation ident "add"
    Latte.Abs.Decr p ident -> do
      commonDecrIncrOperation ident "sub"

    Latte.Abs.Ret p expr -> do
      e <- compilerExpr expr
      exprWithType <- combineTypeAndIndentOfExpr expr e
      let returnText = "ret" ++ " " ++ exprWithType
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [returnText] }

    -- Latte.Abs.VRet p -> ""
    Latte.Abs.SExp _ expr -> do
      e  <- compilerExpr expr
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [e] }
    other -> throwError $ "Not implemented: " ++ show other

commonDecrIncrOperation ident op = do
  s <- get
  let varName = name ident
  case Map.lookup varName (compilerVariables s) of
    Just varType -> do
      if Map.member varName (arguments s) then do
        counter <- getNextIndirectVariableAndUpdate
        let argOperation = "%" ++ show counter ++ " = " ++ op ++ " " ++ typeToLlvmKeyword varType ++ " %" ++ varName ++ ", 1"
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [argOperation] }
      else do
        -- Obsługa zmiennej lokalnej
        counter <- getNextIndirectVariableAndUpdate
        let loadInstr = "%" ++ show counter ++ " = load " ++ typeToLlvmKeyword varType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varName
        nextCounter <- getNextIndirectVariableAndUpdate
        let opInstr = "%" ++ show nextCounter ++ " = " ++ op ++ " " ++ typeToLlvmKeyword varType ++ " %" ++ show counter ++ ", 1"
        let storeInstr = "store " ++ typeToLlvmKeyword varType ++ " %" ++ show nextCounter ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varName
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [loadInstr, opInstr, storeInstr] }
    Nothing -> throwError $ "Variable not defined: " ++ varName



runCompiler :: (Compile a) => a -> Map (Latte.Abs.Ident, [Type]) Type -> Map Latte.Abs.Expr Type -> Either String CompilerState
runCompiler program functionsSignatures exprTypes = execStateT (compile program) $ CompilerState [] Map.empty 0 functionsSignatures exprTypes Map.empty Map.empty