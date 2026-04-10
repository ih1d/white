module Main where

import Parser
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Eval (baseEval)
import Language.Haskell.Interpreter (runInterpreter)

main :: IO ()
main = do
  putStrLn "Welcome to the reflective language Blue! (v0.1)"
  hSetBuffering stdout NoBuffering
  repl

repl :: IO ()
repl = do
  putStr "BLUE> "
  l <- getLine
  case parseExpr l of
    Left err -> print err >> repl
    Right e -> do
      v <- runInterpreter (baseEval e)
      print v
      repl