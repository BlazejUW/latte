{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unwords, unlines
  , Bool(..), (&&), (||), not
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn, putStr
  , FilePath
  , getContents, readFile, return, reverse
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitWith )
import Control.Monad      ( when )
import System.IO
import Data.Foldable      ( elem )

import qualified Latte.Abs
import Latte.Abs   ()
import Latte.Lex   ( Token, mkPosToken )
import Latte.Par   ( pProgram, myLexer )
import Latte.Print ( Print, printTree )
import Latte.Typechecker (runTypechecker, functionsSignatures, exprTypes, inlineFunctions, Typecheck, TypecheckerState)
import Latte.Compiler (Compile, runCompiler, compilerOutput, CompilerState)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Inline     = String

doInlineAllFunctions :: Inline -> Bool
doInlineAllFunctions s = case s of
  "inline" -> True
  _ -> False

runFile :: (Print a, Show a, Compile a, Typecheck a) => Inline -> ParseFun a -> FilePath -> IO ()
runFile inline p f = readFile f >>= run inline p

run :: (Print a, Show a, Compile a, Typecheck a) => Inline -> ParseFun a -> String -> IO ()
run inline p s =
  case p ts of
    Left err -> do
      hPutStrLn stderr "ERROR\n"
      hPutStrLn stderr err
      exitFailure
    Right tree  -> do
      let typecheckResult = runTypechecker tree (doInlineAllFunctions inline)
      case typecheckResult of
        Left err -> do
          hPutStrLn stderr "ERROR\n"
          hPutStrLn stderr "\n## Typechecking Failed...\n"
          hPutStrLn stderr err
          exitFailure
        Right s -> do
          let result = runCompiler tree (functionsSignatures s) (exprTypes s) (inlineFunctions s)
          case result of
            Left err -> do
              hPutStrLn stderr "ERROR\n"
              hPutStrLn stderr "\n## Evaluation Failed...\n"
              hPutStrLn stderr err
              exitFailure
            Right s -> do
              let lines = unlines (compilerOutput s)
              hPutStrLn stderr "OK\n"
              putStr lines
  where
  ts = myLexer s


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin. All not-recursive functions are inlined."
    , "  -no_inline      Parse stdin. None of function is inlined."
    , "  (files)         Parse content of files."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run "inline" pProgram
    "-no_inline":fs -> mapM_ (runFile "no_inline" pProgram) fs
    fs -> mapM_ (runFile "inline" pProgram) fs
