{-# LANGUAGE RecordWildCards #-}
module Amulet.Parser.Lexer
  ( module X
  , langDef
  , ids
  , identifier
  , reserved
  , operator
  , reservedOp
  , charLiteral
  , stringLiteral
  , natOrFloat
  , symbol
  , lexeme
  , parens
  , natural
  , integer
  , floating
  , braces
  , squares
  , angles
  , semi
  , colon
  , comma
  , dot
  , semiSep
  , commaSep
  , semiSep1
  , commaSep1
  , initParseState
  , laidout
  , indentCmp
  , indented
  , align
  , block
  , block1
  , Parser
  , ParserState(..)
  )
  where

import qualified Text.Parsec.Token as T
import Text.Parsec.String ()
import Text.Parsec.Prim as X
import Text.Parsec.Pos as X
import Text.Parsec.Char as X
import Text.Parsec.Combinator as X
import Control.Monad.Identity

import Data.Char

type Parser = Parsec String ParserState

data ParserState
  = ParserState { indents :: Int } deriving (Eq, Ord)

langDef :: T.GenLanguageDef String ParserState Identity
langDef
  = T.LanguageDef { T.commentStart = "(*"
                  , T.commentEnd   = "*)"
                  , T.commentLine  = ";"
                  , T.nestedComments = True
                  , T.identLetter = ids <|> satisfy ((`elem` gcNumbers) . generalCategory) <|> char '\''
                  , T.identStart  = ids
                  , T.opStart     = satisfy ((`elem` gcSymbols) . generalCategory)
                  , T.opLetter    = satisfy ((`elem` gcSymbols) . generalCategory)
                  , T.reservedNames = [ "type", "data", "forall"
                                    , "let", "rec", "left"
                                    , "right", "op", "match"
                                    , "with", "cond", "if"
                                    , "else", "then", "foreign"
                                    , "and", "in", "open"
                                    , "as", "public", "private"
                                    , "internal", "export" ]
                  , T.reservedOpNames = ["->", "=>", "|", "``"]
                  , T.caseSensitive = True }

ids :: Parser Char
ids = satisfy ((`elem` gcLetters) . generalCategory)

lexer :: T.GenTokenParser String ParserState Identity
lexer = T.makeTokenParser langDef

identifier :: Parser String
identifier = T.identifier lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

operator :: Parser String
operator = T.operator lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

charLiteral :: Parser Char
charLiteral = T.charLiteral lexer

stringLiteral :: Parser String
stringLiteral = T.stringLiteral lexer

natOrFloat :: Parser (Either Integer Double)
natOrFloat = T.naturalOrFloat lexer

natural :: Parser Integer
natural = T.natural lexer

integer :: Parser Integer
integer = T.integer lexer

floating :: Parser Double
floating = T.float lexer

symbol :: String -> Parser String
symbol = T.symbol lexer

lexeme :: Parser a -> Parser a
lexeme = T.lexeme lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

braces :: Parser a -> Parser a
braces = T.braces lexer

squares :: Parser a -> Parser a
squares = T.brackets lexer

angles :: Parser a -> Parser a
angles = T.angles lexer

semi :: Parser String
semi = T.semi lexer

colon :: Parser String
colon = T.colon lexer

comma :: Parser String
comma = T.comma lexer

dot :: Parser String
dot = T.dot lexer

semiSep :: Parser a -> Parser [a]
semiSep = T.semiSep lexer

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = T.semiSep1 lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = T.commaSep1 lexer

gcLetters :: [GeneralCategory]
gcLetters = [ UppercaseLetter
            , LowercaseLetter
            , TitlecaseLetter
            , ModifierLetter
            , OtherLetter ]

gcNumbers :: [GeneralCategory]
gcNumbers = [ DecimalNumber
            , LetterNumber
            , OtherNumber ]

gcSymbols :: [GeneralCategory]
gcSymbols = [ MathSymbol
            , CurrencySymbol
            , OtherSymbol ]

initParseState :: ParserState
initParseState = ParserState{..} where
  indents = 0

laidout :: Parser a -> Parser a
laidout m = do
  cur <- indents <$> getState
  pos <- sourceColumn <$> getPosition
  modifyState $ \st -> st { indents = pos }
  res <- m
  modifyState $ \st -> st { indents = cur }
  return res

indentCmp :: (Int -> Int -> Bool) -> Parser ()
indentCmp cmp = do
  col <- sourceColumn <$> getPosition
  current <- indents <$> getState
  guard (col `cmp` current)

indented :: Parser ()
indented = indentCmp (>) <?> "Block (indented further)"

align :: Parser ()
align = indentCmp (==) <?> "Block (same indentation)"

block, block1 :: Parser a -> Parser [a]
block p  = laidout (many  (align >> p))
block1 p = laidout (many1 (align >> p))
