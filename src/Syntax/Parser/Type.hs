module Syntax.Parser.Type (atype) where

import Syntax.Tree
import Syntax.Lexer

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex

atype :: Parser Type
atype =
  forall' <|> ep

ep :: Parser Type
ep = Ex.buildExpressionParser table atype'
  where table = [[Ex.Infix afun Ex.AssocLeft]
                ,[Ex.Infix atup Ex.AssocLeft]]

afun = lexeme $ do reservedOp "->"
                   return TFunc

atup = lexeme $ do reservedOp "*"
                   return $ \x y -> TTuple [x, y]

typevar = lexeme $ do
  char '\''
  x <- many letter
  return $ TVar x
typename = lexeme $ do
  x <- upper
  xs <- many letter
  return $ TIdent $ ScopeName $ x:xs
tinst = lexeme $ do
  tn <- typename
  tv <- angles typevar
  return $ TInst tn tv
forall' = lexeme $ do
  reserved "forall"
  v <- typevar
  reserved "."
  x <- optionMaybe constraints
  t <- chainr1 atype' afun
  let (TVar v') = v in
    return $ case x of
               Just x' -> TForAll v' x' t
               Nothing -> TForAll v' [] t

atype' = parens atype
     <|> try tinst
     <|> typevar
     <|> typename
constraints = single <|> many'
single = lexeme $ do x <- tinst
                     reservedOp "=>"
                     return [x]
many' = lexeme $ do
  x <- parens $ commaSep1 tinst
  reservedOp "=>"
  return x
