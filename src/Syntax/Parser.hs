module Syntax.Parser where

import Syntax.Tree
import Syntax.Lexer


import Text.Parsec.Char
import Text.Parsec.Combinator

import Text.Parsec
import Text.Parsec.Prim

import Text.Parsec.String (Parser)

import Control.Applicative hiding ((<|>), many)

import Debug.Trace (trace)

param :: Parser Expr
param = do
  nam <- identifier
  t1  <- optionMaybe $ colon >> atype
  
  let name = EVar nam

  return $ case t1 of
             Just atyp -> EUpcast name atyp
             Nothing   -> name

atype :: Parser Type
atype = do fs <- upper
           rest <- manyTill lower space
           return $ TIdent $ Name $ fs:rest

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 param
  reservedOp "->"
  body <- expression
  return $ foldr ELambda body args

true, false :: Parser Expr
true = do reserved "true"
          return $ ELiteral $ LBoolean True

false = do reserved "false"
           return $ ELiteral $ LBoolean False

double :: Parser Expr
double = do
  x <- natOrFloat
  return $ case x of
    Right x -> ELiteral $ LNumber x
    Left i -> ELiteral $ LNumber $ fromIntegral i


string :: Parser Expr
string = let escape = do c <- char '\\'
                         d <- oneOf "\\\"0nrvtbf"
                         return [c, d]
             nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"
             character = return <$> nonEscape <|> escape
          in do char '"'
                strs <- many character
                char '"'
                return $ ELiteral $ LString $ concat strs

unit :: Parser Expr
unit = do reservedOp "()"
          return $ ELiteral LUnit


literal :: Parser Expr
literal = do unit
         <|> Syntax.Parser.string
         <|> try double
         <|> true <|> false 

index :: Parser Expr
index = do e <- expression
           char '.'
           n <- identifier
           return $ EIndex e n

tuple :: Parser Expr
tuple = parens $ commaSep1 expression >>= \x -> return $ ETuple x

var :: Parser Expr
var = identifier >>= \x -> return $ EVar x

if' :: Parser Expr
if' = do reserved "if"
         cond <- expression
         reserved "then"
         then' <- expression
         reserved "else"
         else' <- expression
         return $ EIf cond then' else'

letreg :: Parser Expr
letreg = do
  reserved "let"
  binds <- sepBy1 letbind $ reserved "and"
  reserved "in"
  expr <- expression

  return $ ELet False binds expr
  

letrec :: Parser Expr
letrec = do
  reserved "let"
  reserved "rec"
  binds <- sepBy1 letbind $ reserved "and"
  reserved "in"
  expr <- expression

  return $ ELet True binds expr

let' :: Parser Expr
let' = do try letrec
          <|> letreg

letbindimut :: Parser LetBinding
letbindimut = do
  nam <- declaration
  reservedOp "="
  e1 <- expression

  return $ LetBinding nam e1 False

letbindmut :: Parser LetBinding
letbindmut = do
  reserved "mut"
  nam <- declaration
  reservedOp "="
  e1 <- expression

  return $ LetBinding nam e1 False

letbind = try letbindmut
          <|> letbindimut

declaration :: Parser Declaration
declaration = dname
          <|> dtupl
          <|> (char '_' >> return DDiscard)
  where dname = identifier >>= \x -> return $ DName x
        dtupl = parens $ commaSep1 declaration >>= \x -> return $ DTuple x

list :: Parser Expr
list = brackets $ semiSep1 expression >>= \x -> return $ EList x

assign :: Parser Expr
assign = do
  nm <- assignable
  reservedOp "<-"
  e1 <- expression
  reserved "in"
  e2 <- expression

  return $ EAssign nm e1 e2

assignable :: Parser Assignable
assignable = tupl <|> nam
  where tupl = (parens $ commaSep1 assignable) >>= \x -> return $ ATuple x
        nam  = identifier >>= \x -> return $ AName x


term :: Parser Expr
term = Syntax.Parser.var
    <|> lambda
    <|> literal
    <|> tuple
    <|> parens term
    <|> if'
    <|> let'
    <|> list
    <|> assign

expression :: Parser Expr
expression = do
  t <- many1 term
  return $ foldl1 EApply t

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents term) "<stdin>"
