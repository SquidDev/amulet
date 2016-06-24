module Main (main) where

import Analysis.Infer
import Analysis.NameResolver
import Analysis.Scope
import Pretty
import Syntax.Parser
import Types.TypeVar

import Control.Monad
import Data.List (intercalate)
import Data.Maybe
import Syntax.Tree
import System.IO
import Codegen.Lua (Statement(Do))
import Text.Parsec.Error

import Codegen.Codegen
import Codegen.Emit

import Analysis.SymbolTable

import qualified Data.Map as Map

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

basicScope :: Scope
basicScope = (
  TypeScope $ Map.fromList [
      pair "Num",
      pair "Bool",
      pair "String",
      pair "Unit",
      pair "List"
      ],
  ExprScope Map.empty,
  nullModule
  )
  where amu = QualifiedName ["Amulet"]
        root = QualifiedName []
        pair :: Ident -> (Name, Name)
        pair x = (root x, amu x)
main :: IO()
main =
  let item = do
        expr <- parse parseSt
        case expr of
          Left e -> print e
          Right x -> do
            putStrLn $ pshow x
            case runResolver basicScope $ resolveStatement x of
              Left e -> putStrLn $ intercalate "\n" $ map pshow e
              Right x -> do
                let xs = compile $ mkST [x]
                print $ emitS (Do xs)
  in forever item
