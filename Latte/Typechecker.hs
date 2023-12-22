{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Latte.Typechecker where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.State (StateT, execStateT, get, gets, modify, put)
import Data.Map (Map, adjust, delete, insert)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Latte.Abs
import Latte.Helpers (Type (Boolean, Integer, String, Void), errLocation, functionName, keywordToType, name, typeToKeyword)
import Data.Void (Void)

data TypecheckerState = TypecheckerState
  { functions :: Map (Latte.Abs.Ident, [Type]) Latte.Abs.TopDef,
    functionsSignatures :: Map Latte.Abs.Ident (Type, [Type]), 
    variables :: Map Latte.Abs.Ident (Bool, Type), -- Bool is True if variable is mutable
    globalVariables :: Map Latte.Abs.Ident (Bool, Type), -- Bool is True if variable is mutable
    expectedReturnType :: Maybe Type,
    returnReachable :: Bool
  }
  deriving (Eq, Ord, Show)

type LTS a = StateT TypecheckerState (Either String) a
checkTypes :: (MonadError [Char] f, Show a1, Show a2) => [Char] -> Maybe (a1, a2) -> Type -> Type -> f ()
checkTypes ident p l r = unless (l == r) $ throwError $ "Type mismatch for " ++
                 ident ++ " - left (" ++ typeToKeyword l ++ ") and right (" ++
                 typeToKeyword r ++ ") do not match at " ++ errLocation p


class TypecheckExpr a where
  typecheckExpr :: a -> LTS Type

instance TypecheckExpr Latte.Abs.Expr where
  typecheckExpr = \case
    Latte.Abs.ELitInt _ _ -> return Integer
    Latte.Abs.ELitTrue _ -> return Boolean
    Latte.Abs.ELitFalse _ -> return Boolean
    Latte.Abs.EString _ _ -> return String
    Latte.Abs.EAdd p l op r -> do
      lType <- typecheckExpr l
      rType <- typecheckExpr r
      checkTypes "arithmethics operator" p lType rType
      return lType
    Latte.Abs.EMul p l op r -> do
      lType <- typecheckExpr l
      rType <- typecheckExpr r
      checkTypes "arithmethics operator" p lType rType
      return lType
    Latte.Abs.Neg p expr -> do
      t <- typecheckExpr expr
      case t of
        Integer -> return Integer
        other -> throwError $ "Type mismatch for unary minus (expected int but got " ++ typeToKeyword other ++ ") " ++ errLocation p
    Latte.Abs.EAnd p l r -> do
      lType <- typecheckExpr l
      rType <- typecheckExpr r
      checkTypes "&&" p lType rType
      return Boolean
    Latte.Abs.EOr p l r -> do
      lType <- typecheckExpr l
      rType <- typecheckExpr r
      checkTypes "||" p lType rType
      return Boolean
    Latte.Abs.Not p expr -> do
      t <- typecheckExpr expr
      case t of
        Boolean -> return Boolean
        other -> throwError $ "Type mismatch for unary not (expected boolean but got " ++ typeToKeyword other ++ ") " ++ errLocation p
    Latte.Abs.ERel p l op r -> do
      lType <- typecheckExpr l
      rType <- typecheckExpr r
      checkTypes "comparison operator" p lType rType
      return Boolean
    Latte.Abs.EVar p ident -> do
      s <- get
      let localVar = Map.lookup ident (variables s)
      let globalVar = Map.lookup ident (globalVariables s)
      case (localVar, globalVar) of
        (Just (_, type_), _) -> return type_
        (_, Just (_, type_)) -> return type_
        _ -> throwError $ "Variable " ++ name ident ++ " not found " ++ errLocation p
    Latte.Abs.EApp p ident exprs -> do
      s <- get
      types <- mapM typecheckExpr exprs
      let key = (ident, types)
      case Map.lookup ident (functionsSignatures s) of
        Nothing -> throwError $ "Function " ++ functionName key ++ " not found " ++ errLocation p
        Just (t, expectedTypes) -> do
              actualTypes <- mapM typecheckExpr exprs
              unless (expectedTypes == actualTypes) $ throwError $ "Function " ++ name ident ++ " called with wrong arguments " ++ errLocation p
              return t


class Typecheck a where
  typecheck :: a -> LTS ()

collectFunctionSignatures :: Latte.Abs.TopDef -> LTS ()
collectFunctionSignatures (Latte.Abs.FnDef _ t ident args _) = do
  let argTypes = map (\(Latte.Abs.Arg _ argType _) -> keywordToType argType) args
  let returnType = keywordToType t
  modify $ \s -> s {functionsSignatures = Map.insert ident (returnType, argTypes) (functionsSignatures s)}


instance Typecheck Latte.Abs.Program where
  typecheck (Latte.Abs.Program p topdefs) = do
    mapM_ collectFunctionSignatures topdefs
    mapM_ typecheck topdefs
    functions <- gets functionsSignatures
    let key = Latte.Abs.Ident "main"
    when (Map.notMember key functions) $ throwError $ "Function main not found " ++ errLocation p



-- instance Typecheck Latte.Abs.TopDef where
--   typecheck fndef@(Latte.Abs.FnDef p t ident args block) = do
--     -- typecheck block, but first add arguments as variables, and check if return type matches
--     modify $ \s -> s {variables = Map.fromList $ map (\(Latte.Abs.Arg _ type_ ident) -> (ident, (True, keywordToType type_))) args}
--     modify $ \s -> s {expectedReturnType = Just $ keywordToType t}
--     modify $ \s -> s {returnReachable = False}
--     typecheck block
--     returnReached <- gets returnReachable
--     unless returnReached $ throwError $ "Return statement not reachable in function " ++ name ident
--     modify $ \s -> s {expectedReturnType = Nothing}

instance Typecheck Latte.Abs.TopDef where
  typecheck fndef@(Latte.Abs.FnDef p t ident args block) = do
    let argTypes = map (\(Latte.Abs.Arg _ argType _) -> keywordToType argType) args
    let returnType = keywordToType t
    modify $ \s -> s {functionsSignatures = Map.insert ident (returnType, argTypes) (functionsSignatures s)}
    -- typecheck block, but first add arguments as variables, and check if return type matches
    modify $ \s -> s {variables = Map.fromList $ map (\(Latte.Abs.Arg _ type_ ident) -> (ident, (True, keywordToType type_))) args}
    modify $ \s -> s {expectedReturnType = Just $ keywordToType t}
    modify $ \s -> s {returnReachable = False}
    typecheck block
    expectedRetType <- gets expectedReturnType
    case expectedRetType of
      Just Void -> return ()
      _ -> do
        returnReached <- gets returnReachable
        unless returnReached $ throwError $ "Return statement not reachable in function " ++ name ident
    modify $ \s -> s {expectedReturnType = Nothing}


instance Typecheck Latte.Abs.Block where
  typecheck (Latte.Abs.Block p stmts) = do
    originalState <- get
    forM_ stmts typecheck
    returnReached <- gets returnReachable
    put originalState
    modify $ \s -> s {returnReachable = returnReached}

instance Typecheck Latte.Abs.Stmt where
  typecheck = \case
    Latte.Abs.Empty _ -> return ()
    Latte.Abs.BStmt _ block -> typecheck block
    Latte.Abs.Decl p type_ items -> forM_ items $ \item -> case item of
      Latte.Abs.NoInit _ ident -> do
        --check if ident is present in declared variables
        variables_used <- gets variables
        when (Map.member ident variables_used) $ throwError $ "Variable " ++ name ident ++ " already defined " ++ errLocation p
        modify $ \s -> s {variables = insert ident (True, keywordToType type_) (variables s)}
      Latte.Abs.Init _ ident expr -> do
        t <- typecheckExpr expr
        checkTypes "declaration" p (keywordToType type_) t
        modify $ \s -> s {variables = insert ident (True, keywordToType type_) (variables s)}
    Latte.Abs.Ass p ident expr -> do
      s <- get
      t <- typecheckExpr expr
      let localVar = Map.lookup ident (variables s)
      let globalVar = Map.lookup ident (globalVariables s)
      let varInfo = if isJust localVar then localVar else globalVar
      case varInfo of
        Nothing -> throwError $ "Variable " ++ name ident ++ " not found " ++ errLocation p
        Just (False, _) -> throwError $ "Cannot assign to a variable " ++ name ident ++ " as it is a constant " ++ errLocation p
        Just (_, type_) -> checkTypes "assignment" p type_ t
    Latte.Abs.Cond p expr stmt -> do
      t <- typecheckExpr expr
      case t of
        Boolean -> do
          originalState <- get
          typecheck stmt
          put originalState
        other -> throwError $ "Type mismatch for if condition (expected boolean but got " ++ typeToKeyword other ++ ") " ++ errLocation p
    Latte.Abs.CondElse p expr stmt1 stmt2 -> do
          t <- typecheckExpr expr
          case t of
            Boolean -> do
              originalState <- get
              originalReachable <- gets returnReachable
              modify $ \s -> s {returnReachable = False}
              typecheck stmt1
              ifReturnReached1 <- gets returnReachable
              put originalState
              modify $ \s -> s {returnReachable = False}
              typecheck stmt2
              ifReturnReached2 <- gets returnReachable
              put originalState
              modify $ \s -> s {returnReachable = (ifReturnReached1 && ifReturnReached2) || originalReachable}
            other -> throwError $ "Type mismatch for if condition (expected boolean but got " ++ typeToKeyword other ++ ") " ++ errLocation p
    Latte.Abs.While p expr stmt -> do --tutaj tez dodac ten return
      t <- typecheckExpr expr
      originalState <- get
      case t of
        Boolean -> do
          typecheck stmt
          put originalState
        other -> throwError $ "Type mismatch for while condition (expected boolean but got " ++ typeToKeyword other ++ ") " ++ errLocation p
    Latte.Abs.Incr p ident -> typecheckDecrIncr p ident
    Latte.Abs.Decr p ident -> typecheckDecrIncr p ident
    Latte.Abs.Ret p expr -> do
      t <- typecheckExpr expr
      expectedReturnType <- gets expectedReturnType
      case expectedReturnType of
        Nothing -> throwError $ "Unexpected return statement " ++ errLocation p
        Just expectedReturnType -> do
          checkTypes "return" p expectedReturnType t
          modify $ \s -> s {returnReachable = True}
    Latte.Abs.VRet p -> do
      expectedReturnType <- gets expectedReturnType
      case expectedReturnType of
        Nothing -> throwError $ "Unexpected return statement " ++ errLocation p
        Just expectedReturnType -> do
          checkTypes "return" p expectedReturnType Latte.Helpers.Void
          modify $ \s -> s {returnReachable = True}

    Latte.Abs.SExp _ expr -> do
      typecheckExpr expr
      return ()

-- common typecheck for Decr. and Incr.
typecheckDecrIncr p ident = do
  s <- get
  let localVar = Map.lookup ident (variables s)
  let globalVar = Map.lookup ident (globalVariables s)
  case (localVar, globalVar) of
    (Nothing, Nothing) -> throwError $ "Variable " ++ name ident ++ " not found " ++ errLocation p
    (Just (False, _), _) -> throwError $ "Variable " ++ name ident ++ " is immutable " ++ errLocation p
    (_, Just (False, _)) -> throwError $ "Variable " ++ name ident ++ " is immutable " ++ errLocation p
    (Just (_, type_), _) -> do
      case type_ of
        Integer -> return ()
        other -> throwError $ "Type mismatch for increment (expected integer but got " ++ typeToKeyword other ++ ") " ++ errLocation p
    (_, Just (_, type_)) -> do
      case type_ of
        Integer -> return ()
        other -> throwError $ "Type mismatch for increment (expected integer but got " ++ typeToKeyword other ++ ") " ++ errLocation p


-- runTypechecker :: (Typecheck a) => a -> Either String TypecheckerState
-- runTypechecker program = execStateT (typecheck program) $ TypecheckerState Map.empty Map.empty Map.empty Map.empty Nothing False

runTypechecker :: (Typecheck a) => a -> Either String TypecheckerState
runTypechecker program = execStateT (typecheck program) initialState
  where
    initialState = TypecheckerState
      { functions = Map.empty
      , functionsSignatures = predefFunctions
      , variables = Map.empty
      , globalVariables = Map.empty
      , expectedReturnType = Nothing
      , returnReachable = False
      }

    predefFunctions = Map.fromList
      [ (Latte.Abs.Ident "printInt", (Latte.Helpers.Void, [Latte.Helpers.Integer]))
      , (Latte.Abs.Ident "printString", (Latte.Helpers.Void, [Latte.Helpers.String]))
      , (Latte.Abs.Ident "error", (Latte.Helpers.Void, []))
      , (Latte.Abs.Ident "readInt", (Latte.Helpers.Integer, []))
      , (Latte.Abs.Ident "readString", (Latte.Helpers.String, []))
      ]
