module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Parser (parseExpr)
import System.IO (hFlush, isEOF, stdout)
import Text.Megaparsec (errorBundlePretty)

banner :: Text
banner = "Blue 0.2.0 — parser REPL. Type :quit to exit."

prompt :: IO ()
prompt = TIO.putStr "blue> " >> hFlush stdout

loop :: IO ()
loop = do
    prompt
    done <- isEOF
    if done
        then TIO.putStrLn ""
        else do
            line <- TIO.getLine
            case T.strip line of
                "" -> loop
                ":quit" -> pure ()
                ":q" -> pure ()
                src -> do
                    case parseExpr "<repl>" src of
                        Left err -> putStr (errorBundlePretty err)
                        Right e -> putStrLn ("=> " <> show e)
                    loop

main :: IO ()
main = TIO.putStrLn banner >> loop
