module Syntax.Lexer where

import qualified Text.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language (emptyDef)

import Text.Parsec.Char
import Text.Parsec.Combinator

import Text.Parsec

import Text.Parsec.String (Parser)

import Syntax.Tree

languageDef :: T.LanguageDef u
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
    where names = [ "true", "false", "function"
                  , "if", "then", "else", "forall"
                  , "and", "let", "rec", "and", "or", "when"
                  , "mut", "in", "match", "with" ]
          ops = ["\\", "->", ","
                , "()", "?>", "=", ":>"
                , "<-", "&&", "||", "|"
                , "=>", "*"]

lexer :: T.TokenParser ()
lexer = T.makeTokenParser languageDef

lexeme :: Parser a -> Parser a
lexeme = T.lexeme lexer

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

contents :: Parser a -> Parser a
contents p = do
  T.whiteSpace lexer
  r <- p
  eof
  return r


checkType :: String -> Name -> Parser Name
checkType xs rv = if valid then return rv else fail "the name of a type must start with uppercase"
  where valid = head xs `elem` ['A'..'Z']

typename = lexeme $ do
  x <- name
  case x of
    ScopeName x' -> checkType x' x
    QualifiedName _ x' -> checkType x' x

name :: Parser Name
name = lexeme $ try qualified <|> scope
  where scope     = ScopeName <$> identifier
        qualified = do
          lis <- sepBy1 identifier dot
          if length lis == 1
             then return $ ScopeName $ head lis
          else let ini = init lis in
                let fin = last lis in
                 return $ QualifiedName ini fin

