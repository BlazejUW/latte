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
    compilerOutput :: CompilerOutput
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
--     Latte.Abs.ELitTrue _ -> ""
--     Latte.Abs.ELitFalse _ -> ""
--     Latte.Abs.EString _ _ -> ""
--     Latte.Abs.EAdd p l op r -> ""
    
--     Latte.Abs.EMul p l op r -> ""
--     Latte.Abs.Neg p expr -> ""
--     Latte.Abs.EAnd p l r -> ""
--     Latte.Abs.EOr p l r -> ""
--     Latte.Abs.Not p expr -> ""
--     Latte.Abs.ERel p l op r -> ""
--     Latte.Abs.EVar p ident -> ""
--     Latte.Abs.EApp p ident exprs -> ""


class Compile a where
  compile :: a -> LCS ()


instance Compile Latte.Abs.Program where
    -- compilation to LLVM IR
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
    compile block



instance Compile Latte.Abs.Block where
    compile (Latte.Abs.Block _ stmts) = do
        modify $ \s -> s { compilerOutput = compilerOutput s ++ ["{"] }
        forM_ stmts compile 
        modify $ \s -> s { compilerOutput = compilerOutput s ++ ["}"] }


instance Compile Latte.Abs.Stmt where
  compile = \case
    Latte.Abs.Empty _ -> return ()
    -- Latte.Abs.BStmt _ block -> ""
    -- Latte.Abs.Decl p type_ items -> forM_ items $ \item -> case item of
    --   Latte.Abs.NoInit _ ident -> ""
    --   Latte.Abs.Init _ ident expr ->""
    -- Latte.Abs.Ass p ident expr -> ""
    -- Latte.Abs.Cond p expr stmt -> ""
    -- Latte.Abs.CondElse p expr stmt1 stmt2 ->    ""
    -- Latte.Abs.While p expr stmt ->     ""
    -- Latte.Abs.Incr p ident -> ""
    -- Latte.Abs.Decr p ident -> ""
    Latte.Abs.Ret p expr -> do
      e <- compilerExpr expr  -- Użyj pattern matchingu, aby uzyskać pierwszy element
      let returnText = "ret " ++ e
      modify $ \s -> s { compilerOutput = compilerOutput s ++ [returnText] }          
      -- expectedReturnType <- gets expectedReturnType
          -- case expectedReturnType of
          --     Just expectedReturnType -> do
          --       let t = typeToLlvmKeyword expectedReturnType
          --       let returnText = "ret " ++ t ++ " " ++ e
          --       modify $ \s -> s { compilerOutput = compilerOutput s ++ [returnText] }
    -- Latte.Abs.VRet p -> ""
    -- Latte.Abs.SExp _ expr -> ""
    other -> throwError $ "Not implemented: " ++ show other

runCompiler :: (Compile a) => a -> Either String CompilerState
runCompiler program = execStateT (compile program) $ CompilerState []