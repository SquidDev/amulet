module Syntax.Parser where

import Syntax.Tree
import Syntax.Lexer

import Text.Parsec
import Text.Parsec.String (Parser)

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
                whiteSpace
                return $ ELiteral $ LString $ concat strs

unit :: Parser Expr
unit = do reservedOp "()"
          return $ ELiteral LUnit


literal :: Parser Expr
literal = unit
         <|> Syntax.Parser.string
         <|> try double
         <|> true <|> false
         <?> "literal"

index :: Parser Expr
index = do e <- expression
           char '.'
           n <- identifier
           return $ EIndex e n

tuple :: Parser Expr
tuple = parens $ ETuple <$> commaSep1 expression

var :: Parser Expr
var = EVar <$> identifier

if' :: Parser Expr
if' = do reserved "if"
         cond <- expression
         reserved "then"
         then' <- expression
         else' <- option (ELiteral LUnit) (reserved "else" >> expression)
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
let' = try letrec
       <|> letreg
       <?> "let expression"

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
          <?> "let binding"

declaration :: Parser Declaration
declaration = build <$> commaSep1 dterm
  where
        ddiscard = char '_' >> whiteSpace >> return DDiscard
        dname = DName <$> identifier
        dparens = parens dtuple
        dterm = ddiscard <|> dname <|> dparens <?> "declaration"
        -- We don't use declaration as we allow empty tuples here
        dtuple = build <$> commaSep dterm

        build :: [Declaration] -> Declaration
        build [] = DDiscard
        build [single] = single
        build tup = DTuple tup

list :: Parser Expr
list = brackets $ EList <$> semiSep1 expression

assign :: Parser Expr
assign = do
  nm <- assignable
  reservedOp "<-"
  e1 <- expression
  reserved "in"
  e2 <- expression

  return $ EAssign nm e1 e2

assignable :: Parser Assignable
assignable = atuple
  where
        aname  = AName <$> identifier
        aparens = parens atuple
        aterm = aname <|> aparens
        atuple = build <$> commaSep1 aterm
        build :: [Assignable] -> Assignable
        build [single] = single
        build tup = ATuple tup

term :: Parser Expr
term =
    lambda
    <|> literal
    <|> tuple
    <|> parens term
    <|> if'
    <|> let'
    <|> list
    <|> try assign
    <|> Syntax.Parser.var
    <?> "expression"

expression :: Parser Expr
expression = do
  t <- many1 term
  return $ foldl1 EApply t

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents term) "<stdin>"
