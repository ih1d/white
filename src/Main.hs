module Main where

import Parser
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Eval

main :: IO ()
main = do
    putStrLn "Welcome to the reflective language Blue! (v0.1)"
    hSetBuffering stdout NoBuffering
    repl

repl :: IO ()
repl = do
    putStr ">>> "
    l <- getLine
    case parseExpr l of
        Left err -> print err >> repl
        Right e -> print (eval e) >> repl
