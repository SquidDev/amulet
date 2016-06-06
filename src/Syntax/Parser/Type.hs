module Syntax.Parser.Type (atype) where

import Syntax.Tree
import Syntax.Lexer

import Text.Parsec

import Infer.Expr

import qualified Text.Parsec.Expr as Ex

atype :: Parser Type
atype =
  (forall' <?> "a constrained type") <|> ep

ep :: Parser Type
ep = Ex.buildExpressionParser table atype'
  where table = [[Ex.Infix afun Ex.AssocRight]
                ,[Ex.Infix atup Ex.AssocLeft]]

afun = lexeme (do reservedOp "->"
                  return TFunc) <?> "a function type"

atup = lexeme (do
  reservedOp "*"
  return $ \x y -> case x of
    (TTuple xs) -> TTuple $ xs ++ [y]
    _ -> TTuple [x, y]) <?> "a tuple type"

typevar = lexeme (do
  char '\''
  x <- many letter
  return $ TVar x) <?> "a type variable"

tinst = lexeme (do
  tn <- TIdent <$> typename
  tv <- angles typevar
  return $ TInst tn tv) <?> "a type instantiation"

tlist = brackets (lexeme (do
  tn <- (TIdent <$> typename) <|> typevar
  return $ TInst typeList tn) <?> "a list type")

forall' = lexeme (do
  reserved "forall"
  v <- typevar
  reserved "."
  x <- optionMaybe constraints
  t <- chainr1 atype' afun
  let (TVar v') = v in
    return $ case x of
               Just x' -> TForAll v' x' t
               Nothing -> TForAll v' [] t) <?> "a constrained type"

atype' = lexeme $ parens ep
     <|> try tinst
     <|> typevar
     <|> TIdent <$> typename
     <|> tlist

constraints = single <|> many'
single = lexeme $ do x <- tinst
                     reservedOp "=>"
                     return [x]
many' = lexeme $ do
  x <- parens $ commaSep1 tinst
  reservedOp "=>"
  return x
