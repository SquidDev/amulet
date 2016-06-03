module Main (main) where

import Syntax.Parser
import Infer.Run
import Infer
import Pretty

import System.IO
import Control.Monad

import Syntax.Tree

prompt :: String -> IO String
prompt x = do
  putStr x
  hFlush stdout
  getLine

main :: IO()
main =
  let item = do
        msg <- prompt "> "
        case parseSt msg of
          Right x -> case x of 
              SExpr x' -> do
                putStrLn $ pshow x'
                case inferExpr nullEnv x' of
                     Left e -> putStrLn $ pshow e
                     Right x -> putStrLn $ pshow x
              e@STypeDef{} -> putStrLn $ pshow e
              e@SModule{}  -> putStrLn $ pshow e
          Left e -> print e
  in forever item
