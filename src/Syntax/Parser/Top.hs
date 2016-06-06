module Syntax.Parser.Top where

import Syntax.Tree
import Syntax.Lexer

import Text.Parsec

import Infer.Expr

import qualified Text.Parsec.Expr as Ex

import Syntax.Parser.Type


import' :: Parser Import
import' = try with 
      <|> try as
      <|> named

as :: Parser Import
as = do
  reserved "open"
  x <- typename
  reserved "as"
  y <- typename


  return $ INamed x $ case y of
    ScopeName nm -> nm
    QualifiedName _ nm -> nm

with :: Parser Import
with = do
  reserved "open"
  x <- typename
  reserved "with"
  y <- parens $ commaSep1 $ do
    x <- identifier
    reserved "as"
    y <- identifier
    return (x, y)
  
  return $ IPartial x y

named :: Parser Import
named = do
  reserved "open"
  x <- typename
  return $ IAll x

typedef :: Parser TypeDef
typedef = try union <|> record <|> alias

alias :: Parser TypeDef
alias = do
  y <- atype

  return $ TDAlias y

union :: Parser TypeDef
union = TDUnion <$> sepBy1 union' (reservedOp "|")
  where union' = do x <- typename
                    reserved "of"
                    y <- atype
                    
                    let x' = case x of
                               ScopeName n -> n
                               QualifiedName _ n -> n
                    
                    return (x', y)

record :: Parser TypeDef
record = do 
  x <- braces $ semiSep1 recordRow
  return $ TDRecord x

recordRow :: Parser RecordRow
recordRow = try mut <|> imut

imut = do (x, ty, y) <- shared
          return $ RecordRow x ty False y
mut = do reserved "mut"
         (x, ty, y) <- shared
         return $ RecordRow x ty True y

shared = do y <- optionMaybe accessL 
            x <- identifier
            colon
            ty <- atype
            case y of 
              Just ac -> return (x, ty, ac)
              Nothing -> return (x, ty, Public)


accessL = priv <|> pub <|> intern
  where priv = reserved "private" >> return Private
        pub = reserved "public" >> return Public
        intern = reserved "internal" >> return Internal

typedefst = do
  reserved "type"
  x <- typename
  let x' = case x of
              ScopeName n -> n
              QualifiedName _ n -> n
  reservedOp "="
  y <- typedef
  return $ [(x', y)]

