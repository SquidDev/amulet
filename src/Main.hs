module Main (main) where

import Syntax.Parser
import Infer.Run
import Infer
import PrettyPrint

import System.IO
import Control.Monad

prompt :: String -> IO String
prompt x = do
  putStr x
  hFlush stdout
  getLine

main :: IO()
main =
  let item = do
        msg <- prompt "> "
        case parseExpr msg of
          Right x -> do
            putStrLn $ pshow x
            case inferExpr nullEnv x of
              Left e -> putStrLn $ pshow e
              Right x -> putStrLn $ pshow x
          Left e -> print e
  in forever item
