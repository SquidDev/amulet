module Syntax.Lexer where

import qualified Text.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language (emptyDef)

import Text.Parsec.Char
import Text.Parsec.Combinator

import Text.Parsec
import Text.Parsec.Prim

import Text.Parsec.String (Parser)

languageDef = emptyDef
  { T.commentStart    = "(*"
  , T.commentEnd      = "*)"
  , T.commentLine     = "--"
  , T.nestedComments  = True
  , T.identStart      = letter    <|> char '_'
  , T.identLetter     = alphaNum  <|> char '_' 
  , T.reservedNames   = names
  , T.caseSensitive   = True
  , T.reservedOpNames = ops }
    where names = [] 
          ops   = []

lexer :: T.TokenParser ()
lexer = T.makeTokenParser languageDef

identifier :: Parser String
identifier = T.identifier lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

operator :: Parser String
operator = T.operator lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

natural :: Parser Integer
natural = T.natural lexer

integer :: Parser Integer
integer = T.integer lexer

float :: Parser Double
float = T.float lexer

natOrFloat :: Parser (Either Integer Double)
natOrFloat = T.naturalOrFloat lexer

symbol :: String -> Parser String
symbol = T.symbol lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

braces :: Parser a -> Parser a
braces = T.braces lexer

brackets :: Parser a -> Parser a
brackets = T.brackets lexer

angles :: Parser a -> Parser a
angles = T.angles lexer

semi :: Parser String
semi = T.semi lexer

comma :: Parser String
comma = T.comma lexer

colon :: Parser String
colon = T.colon lexer

dot :: Parser String
dot = T.dot lexer

semiSep :: Parser a -> Parser [a]
semiSep = T.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = T.semiSep1 lexer

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = T.commaSep1 lexer
