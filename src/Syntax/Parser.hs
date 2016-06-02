module Syntax.Parser where

import Syntax.Tree
import Syntax.Lexer

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex

import qualified Debug.Trace as D
import Syntax.Parser.Type

debug = flip D.trace

param :: Parser (Ident, Maybe Type)
param = untyped <|> typed
  where untyped = identifier >>= \x -> return (x, Nothing)
        typed = parens $ do
          nam <- identifier
          t1  <- optionMaybe $ colon >> atype

          return (nam, t1)


lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 param
  reservedOp "->"
  body <- expression
  return $ foldr (\(n, ty) e -> ELambda n ty e) body args

true, false :: Parser Literal
true = do reserved "true"
          return $ LBoolean True

false = do reserved "false"
           return $ LBoolean False

double :: Parser Literal
double = do
  x <- natOrFloat
  return $ case x of
    Right x ->  LNumber x
    Left i ->  LNumber $ fromIntegral i


string :: Parser Literal
string = let escape = do c <- char '\\'
                         d <- oneOf "\\\"0nrvtbf"
                         return [c, d]
             nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"
             character = return <$> nonEscape <|> escape
          in do char '"'
                strs <- many character
                char '"'
                whiteSpace
                return $ LString $ concat strs

unit :: Parser Literal
unit = do reservedOp "()"
          return LUnit


literal :: Parser Literal
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
var = EVar . ScopeName <$> identifier

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

letbind :: Parser LetBinding
letbind = try letbindmut
          <|> letbindimut
          <?> "let binding"

declaration :: Parser Declaration
declaration = build <$> commaSep1 dterm
  where
        ddiscard = char '_' >> whiteSpace >> return DDiscard
        dname = DName <$> identifier
        dparens = parens dtuple
        dterm = ddiscard <|> dname <|> dparens <|> drecord <?> "declaration"
        -- We don't use declaration as we allow empty tuples here
        dtuple = build <$> commaSep dterm

        drecord = braces $ semiSep1 record' >>= \x -> return $ DRecord x
        record' = do x <- ScopeName <$> identifier
                     reservedOp "="
                     y <- declaration
                     return (x, y)

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
    <|> (ELiteral <$> literal)
    <|> let'
    <|> tuple
    <|> parens term
    <|> if'
    <|> list
    <|> try assign
    <|> match
    <|> function
    <|> Syntax.Parser.var
    <?> "expression"

downcastop :: Expr -> Expr -> Expr
downcastop x (EVar (ScopeName n))
  | head n `elem` ['A'..'Z'] = EDowncast x $ TIdent $ ScopeName n
  | otherwise                = x
downcastop x _ = x

upcastop :: Expr -> Expr -> Expr
upcastop x (EVar (ScopeName n))
  | head n `elem` ['A'..'Z'] = EUpcast x $ TIdent $ ScopeName n
  | otherwise                = x
upcastop x _ = x

term' :: Parser Expr
term' = Ex.buildExpressionParser table term
  where table = [[ binary "?>" downcastop Ex.AssocLeft
                 , binary ":>" upcastop Ex.AssocLeft ]
                ,[ Ex.Infix dynamicBinOp Ex.AssocLeft ]]
        binary wrd fn = Ex.Infix (reserved wrd >> return fn)

dynamicBinOp :: Parser (Expr -> Expr -> Expr)
dynamicBinOp = infix' <|> op
  where infix' = lexeme $ do
                    char '`'
                    x <- identifier
                    char '`'
                    return $ EBinOp (EVar $ ScopeName x)
        op = lexeme $ do
          x <- operator
          return $ EBinOp (EVar $ ScopeName x)



expression :: Parser Expr
expression = do
  t <- many1 term'
  return $ foldl1 EApply t

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expression) "<stdin>"


matchp' :: Parser Pattern
matchp' = wildcard
      <|> plit
      <|> ptup
      <|> plst
      <|> pbound
      <|> capture
      <?> "match pattern"
  where wildcard = reservedOp "_" >> return PWildcard
        capture  = PCapture <$> identifier
        ptup     = PTuple <$> parens (commaSep1 matchp)
        plst     = PList <$> brackets (semiSep1 matchp)
        plit     = PLiteral <$> literal
        pbound = do
          x <- identifier
          reservedOp "@"
          p <- parens matchp
          return $ PPattern (ScopeName x) p

matchp :: Parser Pattern
matchp = Ex.buildExpressionParser table matchp'
  where binary nm rv = Ex.Infix (reserved nm >> rv) Ex.AssocLeft
        binaryOp nm rv = Ex.Infix (reservedOp nm >> rv) Ex.AssocLeft

        table = [[ binaryOp "|"  $ return por
                 , binaryOp "&"  $ return pand ]]
        por x y = POr [x, y]
        pand x y = PAnd [x, y]
matcharm :: Parser (Pattern, Expr)
matcharm = (do
  reservedOp "|"
  pat <- matchp
  reservedOp "->"
  exp <- expression

  return (pat, exp)) <?> "match arm"

match :: Parser Expr
match = do
  reserved "match"
  e1 <- expression
  reserved "with"
  ps <- many1 matcharm

  return $ EMatch ps e1

function :: Parser Expr
function = do
  reserved "function"
  ps <- many1 matcharm
  return $ ELambda "x" Nothing $ EMatch ps (EVar $ ScopeName "x")


typedef :: Parser TypeDef
typedef = tdalias
  where tdalias = do reserved "type"
                     t1 <- typename
                     reserved "="
                     t2 <- atype
                     return $ TDAlias t1 t2


typename = lexeme $ do
  x <- upper
  xs <- many letter
  return $ ScopeName $ x:xs
