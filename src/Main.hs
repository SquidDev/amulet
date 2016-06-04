module Main (main) where

import Syntax.Parser
import Infer.Run
import Infer
import Pretty

import Control.Monad
import Data.Maybe
import System.IO
import Text.Parsec.Error
import Syntax.Tree

prompt :: String -> IO String
prompt x = do
  putStr x
  hFlush stdout
  getLine

parse :: (String -> Either ParseError a) -> IO (Either ParseError a)
parse parser = readLine Nothing
  where -- readLine :: Maybe String -> IO (Either ParseError a)
        readLine existing = do
          read <- prompt "> "
          let text = case existing of
                      Nothing -> read
                      (Just exis) -> exis ++ "\n" ++ read
          case parser text of
            (Left e) -> if any hasEOF (errorMessages e) && worksNewLine text then
                           readLine $ Just text
                         else
                           return $ Left e
            (Right x) -> if isJust existing && read /= "" then
                          readLine $ Just text
                        else
                          return $ Right x
        -- | Checks if a parser wasn't expecting an EOF
        hasEOF :: Message -> Bool
        hasEOF (SysUnExpect "") = True
        hasEOF _ = False
        -- | Ensures it will still parse if a new line is added
        -- It makes no sense to allow a new line if it will just break
        -- Strings, for instance, will not work: "foobar\n"
        worksNewLine :: String -> Bool
        worksNewLine text =
          case parser $ text ++ "\n" of
            Right _ -> True -- Wat? Parsed with no new line but works with
            Left e -> any hasEOF $ errorMessages e
main :: IO()
main =
  let item = do
        expr <- parse parseSt
        case expr of
          Right(SExpr x) -> do
            putStrLn $ pshow x
            case inferExpr nullEnv x of
              Left e -> putStrLn $ pshow e
              Right x -> putStrLn $ pshow x
          Right(e@STypeDef{}) -> putStrLn $ pshow e
          Right(e@SModule{}) -> putStrLn $ pshow e
          Left e -> print e
  in forever item
