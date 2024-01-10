{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant bracket" #-}

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
  {
    functionsSignatures :: Map (Latte.Abs.Ident, [Type]) Type,
    inlineFunctions :: Map (Latte.Abs.Ident, [Type]) Type,
    variablesStack :: [Map Latte.Abs.Ident Type],
    expectedReturnType :: Maybe Type,
    returnReachable :: Bool,
    exprTypes :: Map Latte.Abs.Expr Type,
    arguments :: Map Latte.Abs.Ident Type,
    isCurrentFunctionInline :: Bool,
    currentFunctionName :: Latte.Abs.Ident
  }
  deriving (Eq, Ord, Show)

type LTS a = StateT TypecheckerState (Either String) a
checkTypes :: (MonadError [Char] f, Show a1, Show a2) => [Char] -> Maybe (a1, a2) -> Type -> Type -> f ()
checkTypes ident p l r = unless (l == r) $ throwError $ "Type mismatch for " ++
                 ident ++ " - left (" ++ typeToKeyword l ++ ") and right (" ++
                 typeToKeyword r ++ ") do not match at " ++ errLocation p

pushFrame :: LTS ()
pushFrame = modify $ \s -> s {variablesStack = Map.empty : variablesStack s}

popFrame :: LTS ()
popFrame = modify $ \s -> s {
  variablesStack = case variablesStack s of
      (_:rest) -> rest
      [] -> []
  }

addVariableToFrame :: Latte.Abs.Ident -> Type -> LTS ()
addVariableToFrame ident t = modify $ \s ->
  let (currentFrame:rest) = variablesStack s
  in s { variablesStack = insert ident t currentFrame : rest }

lookupVariable :: Latte.Abs.Ident -> LTS (Maybe Type)
lookupVariable ident = gets $ \s ->
  let search [] = Nothing
      search (frame:rest) = case Map.lookup ident frame of
        Nothing -> search rest
        justType -> justType
  in search (variablesStack s)

lookupVariableInCurrentFrame :: Latte.Abs.Ident -> LTS (Maybe Type)
lookupVariableInCurrentFrame ident = gets $ \s ->
  let (currentFrame:rest) = variablesStack s
  in Map.lookup ident currentFrame

checkIfFunctionIsInline :: Latte.Abs.Type -> Latte.Abs.Ident -> [Latte.Abs.Arg] -> LTS Bool
checkIfFunctionIsInline t ident args = do
  let name = Latte.Helpers.name ident
  if take 8 name == "inline__" then do
    let argTypes = map (\(Latte.Abs.Arg _ argType _) -> keywordToType argType) args
    let returnType = keywordToType t
    modify $ \s -> s {functionsSignatures = Map.insert (ident, argTypes) returnType (functionsSignatures s)}
    return True
  else return False

isPredefFunction :: Latte.Abs.Ident -> Bool
isPredefFunction ident = do
  Map.member ident predefFunctions
  where
    predefFunctions = Map.fromList
      [ (Latte.Abs.Ident "printInt", Latte.Helpers.Void)
      , ((Latte.Abs.Ident "printString"), Latte.Helpers.Void)
      , (Latte.Abs.Ident "error", Latte.Helpers.Void)
      , ((Latte.Abs.Ident "readInt"), Latte.Helpers.Integer)
      , ((Latte.Abs.Ident "readString"), Latte.Helpers.String)
      ]

class TypecheckExpr a where
  typecheckExpr :: a -> LTS Type

instance TypecheckExpr Latte.Abs.Expr where
  typecheckExpr node = case node of
    Latte.Abs.ELitInt _ _ -> do
      modify $ \s -> s {exprTypes = Map.insert node Integer (exprTypes s)}
      return Integer
    Latte.Abs.ELitTrue _ -> do
      modify $ \s -> s {exprTypes = Map.insert node Boolean (exprTypes s)}
      return Boolean
    Latte.Abs.ELitFalse _ -> do
      modify $ \s -> s {exprTypes = Map.insert node Boolean (exprTypes s)}
      return Boolean
    Latte.Abs.EString _ _ -> do
      modify $ \s -> s {exprTypes = Map.insert node String (exprTypes s)}
      return String
    Latte.Abs.EAdd p l op r -> do
      lType <- typecheckExpr l
      rType <- typecheckExpr r
      checkTypes "arithmethics operator" p lType rType
      modify $ \s -> s {exprTypes = Map.insert node lType (exprTypes s)}
      return lType
    Latte.Abs.EMul p l op r -> do
      lType <- typecheckExpr l
      rType <- typecheckExpr r
      checkTypes "arithmethics operator" p lType rType
      case lType of
        Integer -> do
          modify $ \s -> s {exprTypes = Map.insert node Integer (exprTypes s)}
          return Integer
        _ -> throwError $ "Type mismatch for arithmetic operator (expected integer but got " ++ typeToKeyword lType ++ ") " ++ errLocation p
    Latte.Abs.Neg p expr -> do
      t <- typecheckExpr expr
      case t of
        Integer -> do
          modify $ \s -> s {exprTypes = Map.insert node Integer (exprTypes s)}
          return Integer
        other -> throwError $ "Type mismatch for unary minus (expected int but got " ++ typeToKeyword other ++ ") " ++ errLocation p
    Latte.Abs.EAnd p l r -> do
      lType <- typecheckExpr l
      rType <- typecheckExpr r
      checkTypes "&&" p lType rType
      case lType of
        Boolean -> do
          modify $ \s -> s {exprTypes = Map.insert node Boolean (exprTypes s)}
          return Boolean
        _ -> throwError $ "Type mismatch for logical operator (expected boolean but got " ++ typeToKeyword lType ++ ") " ++ errLocation p
    Latte.Abs.EOr p l r -> do
      lType <- typecheckExpr l
      rType <- typecheckExpr r
      checkTypes "||" p lType rType
      case lType of
        Boolean -> do
          modify $ \s -> s {exprTypes = Map.insert node Boolean (exprTypes s)}
          return Boolean
        _ -> throwError $ "Type mismatch for logical operator (expected boolean but got " ++ typeToKeyword lType ++ ") " ++ errLocation p
    Latte.Abs.Not p expr -> do
      t <- typecheckExpr expr
      case t of
        Boolean -> do
          modify $ \s -> s {exprTypes = Map.insert node Boolean (exprTypes s)}
          return Boolean
        other -> throwError $ "Type mismatch for unary not (expected boolean but got " ++ typeToKeyword other ++ ") " ++ errLocation p
    Latte.Abs.ERel p l op r -> do
      lType <- typecheckExpr l
      rType <- typecheckExpr r
      checkTypes "comparison operator" p lType rType
      modify $ \s -> s {exprTypes = Map.insert node Boolean (exprTypes s)}
      return Boolean
    Latte.Abs.EVar p ident -> do
      s <- get
      localVar <- lookupVariable ident
      case localVar of
        Just type_ -> do
          modify $ \s -> s {exprTypes = Map.insert node type_ (exprTypes s)}
          return type_
        _ -> throwError $ "Variable " ++ name ident ++ " not found " ++ errLocation p
    Latte.Abs.EApp p ident exprs -> do
      s <- get
      isInline <- gets isCurrentFunctionInline
      when (isInline && not (isPredefFunction ident)) $ do
        currentFunctionName <- gets currentFunctionName
        throwError $ "Inline function " ++ name currentFunctionName ++ " cannot call other functions: " ++ name ident ++ " " ++ errLocation p
      types <- mapM typecheckExpr exprs
      let key = (ident, types)
      case Map.lookup key (functionsSignatures s) of
        Nothing -> throwError $ "Function " ++ functionName key ++ " not found " ++ errLocation p
        Just t-> do
          modify $ \s -> s {exprTypes = Map.insert node t (exprTypes s)}
          return t

class Typecheck a where
  typecheck :: a -> LTS ()

collectFunctionSignatures :: Latte.Abs.TopDef -> LTS ()
collectFunctionSignatures (Latte.Abs.FnDef _ t ident args _) = do
  let argTypes = map (\(Latte.Abs.Arg _ argType _) -> keywordToType argType) args
  let returnType = keywordToType t
  modify $ \s -> s {functionsSignatures = Map.insert (ident, argTypes) returnType (functionsSignatures s)}


instance Typecheck Latte.Abs.Program where
  typecheck (Latte.Abs.Program p topdefs) = do
    mapM_ collectFunctionSignatures topdefs
    mapM_ typecheck topdefs
    functions <- gets functionsSignatures
    let key = (Latte.Abs.Ident "main", [])
    when (Map.notMember key functions) $ throwError $ "Function main not found " ++ errLocation p

instance Typecheck Latte.Abs.TopDef where
  typecheck fndef@(Latte.Abs.FnDef p t ident args block) = do
    let argTypes = map (\(Latte.Abs.Arg _ argType _) -> keywordToType argType) args
    let returnType = keywordToType t
    isInline <- checkIfFunctionIsInline t ident args
    modify $ \s -> s {isCurrentFunctionInline = isInline}
    modify $ \s -> s {functionsSignatures = Map.insert (ident, argTypes) returnType (functionsSignatures s)}
    modify $ \s -> s {currentFunctionName = ident}
    pushFrame
    forM_ args $ \(Latte.Abs.Arg _ type_ ident) -> do
      addVariableToFrame ident (keywordToType type_)
      modify $ \s -> s {arguments = insert ident (keywordToType type_) (arguments s)}
    modify $ \s -> s {expectedReturnType = Just $ keywordToType t, returnReachable = False}
    typecheck block
    expectedRetType <- gets expectedReturnType
    case expectedRetType of
      Just Void -> return ()
      _ -> do
        returnReached <- gets returnReachable
        unless returnReached $ throwError $ "Return statement not reachable in function " ++ name ident
    popFrame
    modify $ \s -> s {arguments = Map.empty}
    modify $ \s -> s {expectedReturnType = Nothing}


instance Typecheck Latte.Abs.Block where
  typecheck (Latte.Abs.Block p stmts) = do
    forM_ stmts typecheck

instance Typecheck Latte.Abs.Stmt where
  typecheck = \case
    Latte.Abs.Empty _ -> return ()
    Latte.Abs.BStmt _ block -> do
      s <- get
      pushFrame
      forM_ (Map.toList $ arguments s) $ \(ident, type_) -> do
        addVariableToFrame ident type_
      typecheck block
      popFrame
    Latte.Abs.Decl p type_ items -> forM_ items $ \item -> case item of
      Latte.Abs.NoInit _ ident -> do
        localVar <- lookupVariableInCurrentFrame ident
        when (isJust localVar) $ throwError $ "Variable " ++ name ident ++ " already defined " ++ errLocation p
        addVariableToFrame ident (keywordToType type_)
      Latte.Abs.Init _ ident expr -> do
        t <- typecheckExpr expr
        checkTypes "declaration" p (keywordToType type_) t
        localVar <- lookupVariableInCurrentFrame ident
        when (isJust localVar) $ throwError $ "Variable " ++ name ident ++ " already defined " ++ errLocation p
        addVariableToFrame ident (keywordToType type_)
    Latte.Abs.Ass p ident expr -> do
      t <- typecheckExpr expr
      localVar <- lookupVariable ident
      case localVar of
        Nothing -> throwError $ "Variable " ++ name ident ++ " not found " ++ errLocation p
        Just type_ -> checkTypes "assignment" p type_ t
    Latte.Abs.Cond p expr stmt -> do
      t <- typecheckExpr expr
      case t of
        Boolean -> do
          originalState <- get
          case expr of
            Latte.Abs.ELitTrue _ -> do
              typecheck stmt
              currentState <- get
              put $ originalState { exprTypes = exprTypes currentState }
              modify $ \s -> s {returnReachable = returnReachable currentState}
            _ -> do
              typecheck stmt
              currentState <- get
              put $ originalState { exprTypes = exprTypes currentState }
        other -> throwError $ "Type mismatch for if condition (expected boolean but got " ++ typeToKeyword other ++ ") " ++ errLocation p
    Latte.Abs.CondElse p expr stmt1 stmt2 -> do
      t <- typecheckExpr expr
      case t of
        Boolean -> do
          originalState <- get
          originalReachable <- gets returnReachable
          modify $ \s -> s {returnReachable = False}
          case expr of
            Latte.Abs.ELitTrue _ -> do
              typecheck stmt1
              ifReturnReached <- gets returnReachable
              typecheck stmt2
              currentState <- get
              put $ originalState { exprTypes = exprTypes currentState }
              modify $ \s -> s {returnReachable = ifReturnReached || originalReachable}
            Latte.Abs.ELitFalse _ -> do
              typecheck stmt1
              currentState <- get
              put $ originalState { exprTypes = exprTypes currentState }
              typecheck stmt2
              elseReturnReached <- gets returnReachable
              currentState <- get
              put $ originalState { exprTypes = exprTypes currentState }
              modify $ \s -> s {returnReachable = elseReturnReached || originalReachable}
            _ -> do
              typecheck stmt1
              ifReturnReached1 <- gets returnReachable
              currentState <- get
              put $ originalState { exprTypes = exprTypes currentState }
              modify $ \s -> s {returnReachable = False}
              typecheck stmt2
              ifReturnReached2 <- gets returnReachable
              currentState <- get
              put $ originalState { exprTypes = exprTypes currentState }
              modify $ \s -> s {returnReachable = (ifReturnReached1 && ifReturnReached2) || originalReachable}
        other -> throwError $ "Type mismatch for if condition (expected boolean but got " ++ typeToKeyword other ++ ") " ++ errLocation p
    Latte.Abs.While p expr stmt -> do
      t <- typecheckExpr expr
      originalState <- get
      case t of
        Boolean -> do
          typecheck stmt
          case expr of
            Latte.Abs.ELitTrue _ -> do
              currentState <- get
              put $ originalState { exprTypes = exprTypes currentState }
              modify $ \s -> s {returnReachable = returnReachable currentState}
            _ -> do
              currentState <- get
              put $ originalState { exprTypes = exprTypes currentState }
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
  localVar <- lookupVariable ident
  case localVar of
    Nothing -> throwError $ "Variable " ++ name ident ++ " not found " ++ errLocation p
    Just type_ -> do
      case type_ of
        Integer -> return ()
        other -> throwError $ "Type mismatch for increment (expected integer but got " ++ typeToKeyword other ++ ") " ++ errLocation p

runTypechecker :: (Typecheck a) => a -> Either String TypecheckerState
runTypechecker program = execStateT (typecheck program) initialState
  where
    initialState = TypecheckerState
      { functionsSignatures = predefFunctions
      ,  inlineFunctions = Map.empty
      , variablesStack = [Map.empty]
      , expectedReturnType = Nothing
      , returnReachable = False
      , exprTypes = Map.empty
      , arguments = Map.empty
      , isCurrentFunctionInline = False,
      currentFunctionName = Latte.Abs.Ident ""
      }

    predefFunctions = Map.fromList
      [ ((Latte.Abs.Ident "printInt", [Latte.Helpers.Integer]), Latte.Helpers.Void)
      , ((Latte.Abs.Ident "printString", [Latte.Helpers.String]), Latte.Helpers.Void)
      , ((Latte.Abs.Ident "error", []), Latte.Helpers.Void)
      , ((Latte.Abs.Ident "readInt", []), Latte.Helpers.Integer)
      , ((Latte.Abs.Ident "readString", []), Latte.Helpers.String)
      ]

