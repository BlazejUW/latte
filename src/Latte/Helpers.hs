{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Latte.Helpers where

import qualified Latte.Abs
import Data.List (intercalate)

data Type = Integer | String | Boolean | Void deriving (Eq, Ord, Show)

typeToKeyword :: Type -> String
typeToKeyword = \case
  String -> "string"
  Integer -> "int"
  Boolean -> "boolean"
  Void -> "void"

keywordToType :: Latte.Abs.Type -> Type
keywordToType = \case
  Latte.Abs.Int _ -> Integer
  Latte.Abs.Str _ -> String
  Latte.Abs.Bool _ -> Boolean
  Latte.Abs.Void _ -> Void

typeToLlvmKeyword :: Type -> String
typeToLlvmKeyword = \case
  Integer -> "i32"
  String -> "i8*"
  Boolean -> "i1"
  Void -> "void"


name :: Latte.Abs.Ident -> String
name (Latte.Abs.Ident s) = s

functionName :: (Latte.Abs.Ident, [Type]) -> String
functionName (Latte.Abs.Ident s, ts) = s ++ "(" ++ intercalate ", " (map show ts) ++ ")"

errLocation :: (Show a1, Show a2) => Maybe (a1, a2) -> String
errLocation p = case p of
  Nothing -> "at unknown location"
  Just (line, col) -> "at line " ++ show line ++ ", column " ++ show col

convertToLlvmChar :: Char -> String
convertToLlvmChar c = case c of
  '\n' -> "\\0A" -- New line
  '\t' -> "\\09" -- Tab
  '\"' -> "\\22" -- Quotation mark
  '\\' -> "\\5C" -- Backslash
  _    -> [c]

convertELitIntToInt :: Latte.Abs.Expr -> Int
convertELitIntToInt expr = case expr of
  Latte.Abs.ELitInt _ i -> fromInteger i
  Latte.Abs.Neg _ e -> - convertELitIntToInt e
  Latte.Abs.EAdd _ e1 op e2 -> case op of
    Latte.Abs.Plus _ -> convertELitIntToInt e1 + convertELitIntToInt e2
    Latte.Abs.Minus _ -> convertELitIntToInt e1 - convertELitIntToInt e2
  Latte.Abs.EMul _ e1 op e2 -> case op of
    Latte.Abs.Times _ -> convertELitIntToInt e1 * convertELitIntToInt e2
    Latte.Abs.Div _ -> convertELitIntToInt e1 `div` convertELitIntToInt e2
    Latte.Abs.Mod _ -> convertELitIntToInt e1 `mod` convertELitIntToInt e2
  _ -> error "convertELitIntToInt: not an integer"

convertListOfStmtToBlockBody :: Latte.Abs.Block -> [Latte.Abs.Stmt] -> Latte.Abs.Block
convertListOfStmtToBlockBody (Latte.Abs.Block p _) newStmts = Latte.Abs.Block p newStmts

createEAdd :: Latte.Abs.BNFC'Position -> Latte.Abs.Ident -> Integer -> Latte.Abs.Expr
createEAdd pos ident num =
    let varExpr = Latte.Abs.EVar pos ident
        intExpr = Latte.Abs.ELitInt pos num
        addOp = Latte.Abs.Plus pos
    in Latte.Abs.EAdd pos varExpr addOp intExpr

getRelOp :: Latte.Abs.RelOp' a -> String
getRelOp op = case op of
  Latte.Abs.LTH _ -> "slt"
  Latte.Abs.LE _ -> "sle"
  Latte.Abs.GTH _ -> "sgt"
  Latte.Abs.GE _ -> "sge"
  Latte.Abs.EQU _ -> "eq"
  Latte.Abs.NE _ -> "ne"

convertToLlvmString :: String -> String
convertToLlvmString s = concatMap convertToLlvmChar s ++ "\\00"

removeLeadingPercent :: String -> String
removeLeadingPercent s = case s of
    ('%':rest) -> rest
    _ -> s

isOpMul :: Latte.Abs.MulOp' a -> Bool
isOpMul op = case op of
  Latte.Abs.Times _ -> False
  _ -> False