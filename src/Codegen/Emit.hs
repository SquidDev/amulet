{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Codegen.Emit where

import Codegen.Lua hiding (block, body)
import Data.List

import Control.Monad.RWS

import Pretty (PParam(..), defaults, colourless)

import Control.Applicative

type Emitter = RWS PParam String Int ()

runEmitter :: Emitter -> PParam -> Int ->  String
runEmitter a b c = let (_, _, ret) = runRWS a b c in ret

colour :: Emit a => Integer -> a -> Emitter
colour clr cmb = do
  x <- asks colours
  tell $ if x
    then "\x1b[1;3" <> show clr <> "m"
    else ""
  y <- emit cmb

  tell $ if x
          then "\x1b[0m"
          else ""
  return y

type' :: Emit a => a -> Emitter
type' x = do
  clr <- asks typeColour
  colour clr x

keyword :: Emit a => a -> Emitter
keyword x = do
  clr <- asks keywordColour
  colour clr x

typevar :: Emit a => a -> Emitter
typevar x = do
  clr <- asks typevarColour
  colour clr x

literal :: Emit a => a -> Emitter
literal x = do
  clr <- asks literalColour
  colour clr x

delim :: Emit a => (String, String) -> a -> Emitter
delim (s, e) y = do
  tell s
  ret <- emit y
  tell e
  return ret

between :: (Emit a, Emit b, Emit c) => a -> b -> c -> Emitter
between a b c = emit a >> emit c >> emit b

parens, braces, squares, angles :: Emit a => a -> Emitter
parens  = delim ("(", ")")
braces  = delim ("{", "}")
squares = delim ("[", "]")
angles  = delim ("<", ">")

quotes, dquotes :: Emit a => a -> Emitter
quotes  = delim ("'", "'")
dquotes = delim ("\"", "\"")

indented :: Emit a => a -> Emitter
indented x = do
  idl <- get
  emit $ concat $ replicate idl "  "
  emit x

block :: Emit a => a -> Emitter
block x = do
  modify $ \s -> s + 1
  emit x
  modify $ \s -> s - 1

body :: Emit a => [a] -> Emitter
body b = do block $ mapM_ (\x -> indented x >> tell ";\n") b

(<+>) :: (Emit a, Emit b) => a -> b -> Emitter
a <+> b = emit a >> emit b
infixl 3 <+>

class Emit a where
  emit :: a -> Emitter

instance Emit String where emit = tell
instance Emit Emitter where emit = id

instance Emit a => Emit (ZipList a) where
  emit xs = do x <- ask
               y <- get
               let (ZipList xs') = xs in
                   emit $ intercalate ", " $ map (\x' -> runEmitter (emit x') x y) xs'

instance Emit Name where
  emit (Scoped n) = emit n
  emit (Qualified ns n) = ZipList ns <+> "." <+> emit n

instance Emit Application where
  emit (Call e@(Name x) a) = e <+> parens (ZipList a)
  emit (Call e a) = parens e <+> parens (ZipList a)
  emit (Invoke t m a) = t <+> ":" <+> m <+> parens (ZipList a)

instance Emit Expr where
  emit Nil = literal "nil"
  emit Dots = literal "..."
  emit Codegen.Lua.True = literal "true"
  emit Codegen.Lua.False = literal "false"
  emit (Table prs) = braces (ZipList prs)
  emit (Tuple e) = emit $ ZipList e
  emit (Index e x) = e <+> squares x
  emit (EApply a) = emit a
  emit (Name n) = emit n
  emit (Number d) = literal d
  emit (String s) = literal $ show s
  emit (Function ars dt b) = do
    let dt' = if dt
        then "..."
        else ""

    keyword "function"
    parens $ ZipList $ filter (not . null) $ ars ++ [dt']
    keyword "\n"
    body b
    indented $ keyword "end"

instance Emit Statement where
  emit (Do xs) = do 
    keyword "do\n"
    body xs
    indented $ keyword "end"

  emit (Set n (Function ars dt b)) = do
    let dt' = if dt
        then "..."
        else ""

    keyword "function "
    emit n
    parens $ ZipList $ filter (not . null) $ ars ++ [dt']
    emit "\n"
    body b
    indented $ keyword "end"

  emit (Set n e) = n <+> " = " <+> e
  emit (While c b) = do
    emit "while " <+> c <+> " do\n"
    body b
    indented $ keyword "end"
  emit (Repeat c b) = do
    keyword "repeat"
    body b
    keyword "until " <+> c

  emit (If (x:xs) els) = do
    mkif x
    mapM_ mkelif xs
    mkels els
    emit "end"
      where mkif (x, b) = do keyword "if " <+> x <+> keyword " then\n"
                             body b
            mkelif (x, b) = keyword "else" >> mkif (x, b)
            mkels [] = tell ""
            mkels b = do keyword "else"
                         body b
  emit (Local nms) = keyword "local " <+> (ZipList $ map fst nms) <+> " = " <+> (ZipList $ map snd nms)
  emit (SApply ap) = emit ap
  emit (Return e) = keyword "return " <+> e
  emit (ForIn its b) = do keyword "for "
                          emit (ZipList $ map fst its)
                          keyword " in "
                          emit (ZipList $ map snd its)
                          keyword " do\n"
                          body b
                          indented $ keyword "end"

  emit (ForRange s e st i b) = do
    keyword "for " <+> i <+> " = " <+> s <+> ", " <+> e <+> ", " <+> st <+> keyword " do"
    body b
    indented $ keyword "end"


instance Emit Double where emit = tell . show
instance (Emit a, Emit b) => Emit (a, b) where
  emit (x, y) = x <+> " = " <+> y

instance Emit Operator where
  emit Add    = emit "+"
  emit Sub    = emit "-"
  emit Mul    = emit "*"
  emit Div    = emit "/"
  emit Mod    = emit "%"
  emit Pow    = emit "^"
  emit Concat = emit ".."
  emit Eq     = emit "=="
  emit Lt     = emit "<"
  emit Le     = emit "<="
  emit And    = emit " and "
  emit Or     = emit " or "
  emit Not    = emit " not "
  emit Len    = emit "#"
  emit Shr    = emit ">>"
  emit Shl    = emit "<<"

runEmitC :: Emitter -> String
runEmitP :: Emitter -> String
runEmitC x = runEmitter x colourless 0
runEmitP x = runEmitter x defaults 0


emitC :: Emit a => a -> String
emitC = runEmitC . emit

emitP :: Emit a => a -> String
emitP = runEmitP . emit
