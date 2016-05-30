module Main (main) where

import Syntax.Parser

import System.Environment 

prompt :: String -> IO String
prompt x = do
  putStr x
  getLine

main = do
  let processInp x = case (parseExpr x) of
                      Right x -> do 
                        print x
                        main
                      Left  e -> print e
    in prompt "> " >>= processInp
