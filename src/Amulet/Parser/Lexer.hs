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

identifier = T.identifier lexer
reserved = T.reserved lexer
operator = T.operator lexer
reservedOp = T.reservedOp lexer
charLiteral = T.charLiteral lexer
stringLiteral = T.stringLiteral lexer
natOrFloat = T.naturalOrFloat lexer
natural = T.natural lexer
integer = T.integer lexer
floating = T.float lexer
symbol = T.symbol lexer
lexeme = T.lexeme lexer 
parens = T.parens lexer
braces = T.braces lexer
squares = T.brackets lexer
angles = T.angles lexer
semi = T.semi lexer
colon = T.colon lexer
comma = T.comma lexer
dot = T.dot lexer
semiSep = T.semiSep lexer
commaSep = T.commaSep lexer
semiSep1 = T.semiSep1 lexer
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


