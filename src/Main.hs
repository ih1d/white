module Main where

import Parser
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Eval 

main :: IO ()
main = do
  putStrLn "Welcome to the reflective language Blue! (v0.1)"
  hSetBuffering stdout NoBuffering
  repl []

repl :: Env -> IO ()
repl env = do
  putStr "BLUE> "
  l <- getLine
  case parseExpr l of
    Left err -> print err >> repl env
    Right e -> do
      mres <- run e env
      case mres of
        Left err -> print err >> repl env
        Right (val, env') -> print val >> repl env'