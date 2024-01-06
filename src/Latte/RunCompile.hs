{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unwords, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn, putStr
  , FilePath
  , getContents, readFile, return, reverse
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitWith )
import Control.Monad      ( when )
import System.IO

import qualified Latte.Abs
import Latte.Abs   ()
import Latte.Lex   ( Token, mkPosToken )
import Latte.Par   ( pProgram, myLexer )
import Latte.Print ( Print, printTree )
--TODO compiler
import Latte.Typechecker (runTypechecker, functionsSignatures, exprTypes, Typecheck, TypecheckerState)
import Latte.Compiler (Compile, runCompiler, compilerOutput, CompilerState)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a, Compile a, Typecheck a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrV v f >> readFile f >>= run v p

run :: (Print a, Show a, Compile a, Typecheck a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrV v "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      hPutStrLn stderr "ERROR\n"
      hPutStrLn stderr err
      exitFailure
    Right tree  -> do
      putStrV v "\nParse Successful!"
      showTree v tree
      let typecheckResult = runTypechecker tree
      case typecheckResult of
        Left err -> do
          hPutStrLn stderr "ERROR\n"
          hPutStrLn stderr "\n## Typechecking Failed...\n"
          hPutStrLn stderr err
          exitFailure
        Right s -> do
          putStrV v "\n## Typechecking Successful!"
          let result = runCompiler tree (functionsSignatures s) (exprTypes s)
          case result of
            Left err -> do
              hPutStrLn stderr "ERROR\n"
              hPutStrLn stderr "\n## Evaluation Failed...\n"
              hPutStrLn stderr err
              exitFailure
            Right s -> do
              putStrV v "\n## Evaluation Successful!"
              putStrV v $ "\n[Final State]\n\n" ++ show s
              putStrV v "\n[Output]"
              let lines = unlines (compilerOutput s)
              hPutStrLn stderr "OK\n"
              putStr lines
              -- exitWith $ exitCode s
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]


showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 0 pProgram
    "-v":fs    -> mapM_ (runFile 2 pProgram) fs
    fs         -> mapM_ (runFile 0 pProgram) fs
