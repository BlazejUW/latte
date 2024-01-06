{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Latte.Helpers where

import qualified Latte.Abs

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
functionName (Latte.Abs.Ident s, ts) = s ++ "(" ++ concatMap show ts ++ ")"

errLocation :: (Show a1, Show a2) => Maybe (a1, a2) -> String
errLocation p = case p of
  Nothing -> "at unknown location"
  Just (line, col) -> "at line " ++ show line ++ ", column " ++ show col

convertToLlvmChar :: Char -> String
convertToLlvmChar c = case c of
  '\n' -> "\\0A" -- Nowa linia
  '\t' -> "\\09" -- Tabulator
  '\"' -> "\\22" -- Cudzysłów
  '\\' -> "\\5C" -- Backslash
  _    -> [c]