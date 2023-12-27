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

data CompilerState = CompilerState
  { 
    compilerOutput :: CompilerOutput,
    compilerVariables :: Map String Type,
    indirectVariablesCounter :: Int,
    functionsSignatures :: Map (Latte.Abs.Ident, [Type]) Type 
    -- expectedReturnType :: Maybe Type
  }
  deriving (Eq, Ord, Show)

type LCS a = StateT CompilerState (Either CompilerError) a
type CompilerError = String
type CompilerOutput = [String]


class CompileExpr a where
  compilerExpr :: a -> LCS String

instance CompileExpr Latte.Abs.Expr where
  compilerExpr = \case
    Latte.Abs.ELitInt _ val -> return ("i32 " ++ show val)
    Latte.Abs.ELitTrue _ -> return "i1 1"
    Latte.Abs.ELitFalse _ -> return "i1 0"
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
      let counter = indirectVariablesCounter s + 1 
      case Map.lookup varName (compilerVariables s) of
        Just varType -> do
          let loadInstr = "%" ++ show counter ++ " = load " ++ typeToLlvmKeyword varType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varName
          modify $ \s -> s { compilerOutput = compilerOutput s ++ [loadInstr], 
                             indirectVariablesCounter = counter}
          return $ typeToLlvmKeyword varType ++ " %" ++ show counter
        Nothing -> throwError $ "Variable not defined: " ++ varName
    -- Latte.Abs.EApp p ident exprs -> do
    --   argsWithTypes <- mapM compilerExpr exprs
    --   let argTypes = map snd argsWithTypes
    --   let argCode = map fst argsWithTypes
    --   let functionName = name ident
    --   s <- get
    --   let signature = case Map.lookup (ident, argTypes) (functionsSignatures s) of
    --                     Just retType -> retType
    --                     Nothing -> error $ "Function signature not found for: " ++ functionName
    --   let callCode = "call " ++ typeToLlvmKeyword signature ++ " @" ++ functionName ++ "(" ++ intercalate ", " argCode ++ ")"
    --   return (typeToLlvmKeyword signature, callCode)
      -- let funName = functionName ident
      -- let funSignature = "@" ++ funName ++ "(" ++ intercalate ", " (map compilerExpr exprs) ++ ")"
      -- let callInstr = "call " ++ typeToLlvmKeyword (keywordToType (Latte.Abs.type_ ident)) ++ " " ++ funSignature
      -- return callInstr


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
    let funName = name ident
    let funSignature =  typeToLlvmKeyword retType ++ " @" ++ funName ++ "(" ++ argsStr ++ ")"
    let funHeader = "define " ++ funSignature
    modify $ \s -> s { compilerOutput = compilerOutput s ++ [funHeader] }
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["{"] }
    compile block
    modify $ \s -> s { compilerOutput = compilerOutput s ++ ["}"] }



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
        let varAssignment = "store " ++ e ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varName
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [varAssignment]}
    Latte.Abs.Ass p ident expr -> do
      s <- get
      let varName = name ident
      case Map.lookup varName (compilerVariables s) of
        Just varType -> do
          e <- compilerExpr expr
          let storeInstr = "store " ++ e ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varName
          modify $ \s -> s { 
            compilerOutput = compilerOutput s ++ [storeInstr],
            compilerVariables = Map.insert varName varType (compilerVariables s)
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
      let returnText = "ret " ++ e
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
      counter <- getNextIndirectVariableAndUpdate
      let loadInstr = "%" ++ show counter ++ " = load " ++ typeToLlvmKeyword varType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varName
      nextCounter <- getNextIndirectVariableAndUpdate
      let opInstr = "%" ++ show nextCounter ++ " = "++ op ++ " " ++ typeToLlvmKeyword varType ++ " %" ++ show counter ++ ", 1"
      let storeInstr = "store " ++ typeToLlvmKeyword varType ++ " %" ++ show nextCounter ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varName
      modify $ \s -> s { 
        compilerOutput = compilerOutput s ++ [loadInstr, opInstr, storeInstr]
      }
    Nothing -> throwError $ "Variable not defined: " ++ varName


runCompiler :: (Compile a) => a -> Map (Latte.Abs.Ident, [Type]) Type -> Either String CompilerState
runCompiler program functionsSignatures = execStateT (compile program) $ CompilerState [] Map.empty 0 functionsSignatures