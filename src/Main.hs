module Main (main) where

import System.IO
import System.Environment
import System.Exit
import Types
import Scanner
import Parser
import Evaluator

evalLoop env (x:xs) = do
  case x of
       PriVal s e -> evalLoop ((PriDef s e) : env) xs
       _          -> do
                        let res = eval x env
                        putStrLn $ show res
                        evalLoop env xs
                        return env
evalLoop env _ = do return env

readInput str env = do
  let ast = primer (alexScanTokens str)
  ext <- evalLoop env ast
  return ext

fromFile fname env = do
  h <- openFile fname ReadMode
  str <- hGetContents h
  ext <- readInput str env
  return ext

fromUser env = do
  putStr "> "
  hFlush stdout
  str <- catch getLine (\e -> exitSuccess)
  ext <- readInput str env
  fromUser ext

loadStandardLib env = do
  ext <- catch (getEnv "PRIMER_LIBRARY_PATH" >>= \s -> fromFile s env)
               (\e -> do
                   putStrLn "------------------------------------------------"
                   putStrLn "  Unable to load standard library. Ensure that  "
                   putStrLn "  environment variable PRIMER_LIBRARY_PATH is   "
                   putStrLn "  pointing to Library.pri file                  "
                   putStrLn "------------------------------------------------"
                   exitFailure)
  return ext

main = do
  ext <- loadStandardLib topLevel
  args <- getArgs
  case args of
       (fname:_) -> fromFile fname ext
       []        -> fromUser ext