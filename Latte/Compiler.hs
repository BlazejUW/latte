-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use lambda-case" #-}

-- module Latte.Typechecker where

-- import Control.Monad.Except (MonadError (throwError))
-- import Control.Monad (forM, forM_, unless, when)
-- import Control.Monad.State (StateT, execStateT, get, gets, modify, put)
-- import Data.Map (Map, adjust, delete, insert)
-- import qualified Data.Map as Map
-- import Data.Maybe (isJust)
-- import qualified Latte.Abs
-- import Latte.Helpers (Type (Boolean, Integer, String, Void), errLocation, functionName, keywordToType, name, typeToKeyword)
-- import Data.Void (Void)

-- data CompilerState = TypecheckerState
--   { 
--     compilerOutput :: CompilerOutput
--   }
--   deriving (Eq, Ord, Show)

-- type LCS a = StateT CompilerState (Either String) a

-- type CompilerOutput = String


-- class CompilerExpr a where
--   compilerExpr :: a -> CompilerOutput

-- instance CompilerExpr Latte.Abs.Expr where
--   compilerExpr = \case
--     Latte.Abs.ELitInt _ ident -> "i32 %" ++ ident
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


-- class Compile a where
--   compile :: a -> LCS ()


-- instance Compile Latte.Abs.Program where
--     -- compilation to LLVM IR
--   compile (Latte.Abs.Program p topdefs) = 


-- instance Compile Latte.Abs.TopDef where
--   compile fndef@(Latte.Abs.FnDef p t ident args block) = ""


-- instance Compile Latte.Abs.Block where
--   compile (Latte.Abs.Block p stmts) = ""

-- instance Compile Latte.Abs.Stmt where
--   compile = \case
--     Latte.Abs.Empty _ -> ""
--     Latte.Abs.BStmt _ block -> ""
--     Latte.Abs.Decl p type_ items -> forM_ items $ \item -> case item of
--       Latte.Abs.NoInit _ ident -> ""
--       Latte.Abs.Init _ ident expr ->""
--     Latte.Abs.Ass p ident expr -> ""
--     Latte.Abs.Cond p expr stmt -> ""
--     Latte.Abs.CondElse p expr stmt1 stmt2 ->    ""
--     Latte.Abs.While p expr stmt ->     ""
--     Latte.Abs.Incr p ident -> ""
--     Latte.Abs.Decr p ident -> ""
--     Latte.Abs.Ret p expr -> ""
--     Latte.Abs.VRet p -> ""
--     Latte.Abs.SExp _ expr -> ""


-- runCompiler :: (Compile a) => a -> Either String CompilerState
-- runCompiler program = execStateT (compile program) initialState
--   where
--     initialState = TypecheckerState
--       { 
--         -- Remove the unnecessary line "CompilerOutput "".
--       }