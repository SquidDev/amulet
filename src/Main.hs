module Main (main) where

import Syntax.Parser
import Infer
import PrettyPrint

import System.IO

prompt :: String -> IO String
prompt x = do
  putStr x
  hFlush stdout
  getLine

main = let processInp x = case parseExpr x of
                      Right x -> do
                        putStrLn $ pshow x
                        main
                      Left e -> do
                        print e
                        main
    in prompt "> " >>= processInp
