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
    compilerVariables :: Map String (Int, Type)
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
--     Latte.Abs.EVar p ident -> ""
--     Latte.Abs.EApp p ident exprs -> ""


getVariableNameWithCounter :: String -> LCS String
getVariableNameWithCounter identifier = do
  state <- get
  let variables = compilerVariables state
  let counter = case Map.lookup identifier variables of
                  Just (count, _) -> count
                  _ -> 0
  let variableName = identifier ++ show counter
  return variableName

getVariableType :: String -> LCS Type
getVariableType identifier = do
  state <- get
  let variables = compilerVariables state
  let variableType = case Map.lookup identifier variables of
                  Just (_, t) -> t
                  _ -> error $ "Variable " ++ identifier ++ " not found"
  return variableType


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
        let counter = 0
        modify $ \s -> s { compilerVariables = Map.insert varName (0, varType) (compilerVariables s)}
        let varDeclaration = "%" ++ varName ++ show counter ++ " = alloca " ++ typeToLlvmKeyword varType
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [varDeclaration] }
      Latte.Abs.Init _ ident expr -> do 
        -- problem, ktory będzie w przyszłości: w llvm nie można używać dwa razy tej samej zmiennej
        -- aktualnie zmienne to po prostu nazwy, ale będzie się to psuło gdy w wewnętrznym bloku będzie zmienna o tej samej nazwie
        -- następnie po wyjściu z bloku przysłonimy jej nazwę deklaracją pozablokową - musimy wtedy zmienić nazwę zmiennej w llvm
        let varName = name ident
        let varType = keywordToType type_
        let counter = 0
        let varNameWithCounter = varName ++ show counter
        modify $ \s -> s { compilerVariables = Map.insert varName (0, varType) (compilerVariables s)}
        let varDeclaration = "%" ++ varNameWithCounter ++ " = alloca " ++ typeToLlvmKeyword varType
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [varDeclaration] }
        e <- compilerExpr expr
        let varAssignment = "store " ++ e ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varNameWithCounter
        modify $ \s -> s { compilerOutput = compilerOutput s ++ [varAssignment]}
    Latte.Abs.Ass p ident expr -> do
      s <- get
      let varName = name ident
      case Map.lookup varName (compilerVariables s) of
        Just (counter, varType) -> do
          let varNameWithCounter = varName ++ show counter
          let newCounter = counter + 1
          let newVarNameWithCounter = varName ++ show newCounter
          e <- compilerExpr expr
          let storeInstr = "store " ++ e ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varNameWithCounter
          let newVarInstr = "%" ++ newVarNameWithCounter ++ " = load " ++ typeToLlvmKeyword varType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varNameWithCounter
          modify $ \s -> s { 
            compilerOutput = compilerOutput s ++ [storeInstr, newVarInstr],
            compilerVariables = Map.insert varName (newCounter, varType) (compilerVariables s)
          }
        Nothing -> throwError $ "Variable not defined: " ++ varName
    -- Latte.Abs.Cond p expr stmt -> ""
    -- Latte.Abs.CondElse p expr stmt1 stmt2 ->    ""
    -- Latte.Abs.While p expr stmt ->     ""
    -- Latte.Abs.Incr p ident -> do
    --   commonDecrIncrOperation ident "add"
    -- Latte.Abs.Decr p ident -> do
    --   commonDecrIncrOperation ident "sub"
      
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
    Just (counter, varType) -> do
      let varNameWithCounter = varName ++ show counter
      let newCounter = counter + 1
      let newVarNameWithCounter = varName ++ show newCounter
      let loadInstr = "%" ++ newVarNameWithCounter ++ " = load " ++ typeToLlvmKeyword varType ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varNameWithCounter
      let addInstr = "%" ++ varNameWithCounter ++ " = "++ op ++ " " ++ typeToLlvmKeyword varType ++ " %" ++ newVarNameWithCounter ++ ", 1"
      let storeInstr = "store " ++ "%" ++ varNameWithCounter ++ ", " ++ typeToLlvmKeyword varType ++ "* %" ++ varNameWithCounter
      modify $ \s -> s { 
        compilerOutput = compilerOutput s ++ [loadInstr, addInstr, storeInstr],
        compilerVariables = Map.insert varName (newCounter, varType) (compilerVariables s)
      }
    Nothing -> throwError $ "Variable not defined: " ++ varName


runCompiler :: (Compile a) => a -> Either String CompilerState
runCompiler program = execStateT (compile program) $ CompilerState [] Map.empty