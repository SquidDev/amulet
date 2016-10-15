{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Pretty where

import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative

import Data.Char
import Data.List

import Debug.Trace

debug = flip trace

type PrettyM = ReaderT PParam (Writer String)
type PrettyP = PrettyM ()

data PParam
  = PParam { colours       :: Bool
           , typeColour    :: String
           , keywordColour :: String
           , typevarColour :: String
           , literalColour :: String
           , delimColour   :: String
           , stringColour  :: String }
    deriving (Eq, Show)

runPrinter :: PParam -> PrettyM a -> String
runPrinter ctx act = let (_, ret) = runWriter (runReaderT act ctx) in ret

defaults :: PParam
defaults = PParam { colours = True
                  , keywordColour = "\x1b[3m"
                  , literalColour = "\x1b[1;33m"
                  , typeColour    = "\x1b[34m"
                  , typevarColour = "\x1b[37m"
                  , delimColour   = "\x1b[1;30m"
                  , stringColour  = "\x1b[32m" }

colourless :: PParam
colourless = defaults { colours = False }

print :: Pretty a => a -> String
print = runPrinter defaults . pprint

ppshow :: Pretty a => PParam -> a -> String
ppshow = (. pprint) . runPrinter

class Pretty a where
  pprint :: a -> PrettyP

colour :: Pretty a => String -> a -> PrettyP
colour clr cmb = do
  x <- asks colours
  when x (tell $ clr)
  y <- pprint cmb
  when x $ tell "\x1b[0m"
  return y

tyClr :: Pretty a => a -> PrettyP
tyClr x = do
  clr <- asks typeColour
  colour clr x

kwClr :: Pretty a => a -> PrettyP
kwClr x = do
  clr <- asks keywordColour
  colour clr x

tvClr :: Pretty a => a -> PrettyP
tvClr x = do
  clr <- asks typevarColour
  colour clr x

litClr :: Pretty a => a -> PrettyP
litClr x = do
  clr <- asks literalColour
  colour clr x

strClr :: Pretty a => a -> PrettyP
strClr x = do
  clr <- asks stringColour
  colour clr x

delClr :: Pretty a => a -> PrettyP
delClr x = do
  clr <- asks delimColour
  colour clr x


delim :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> PrettyP
delim s e y = (delClr s) <+> y <+> (delClr e)

parens, braces, squares, angles :: Pretty a => a -> PrettyP
parens  = delim "(" ")"
braces  = delim "{" "}"
squares = delim "[" "]"
angles  = delim "<" ">"

quotes, dquotes :: Pretty a => a -> PrettyP
quotes  = delim "'" "'"
dquotes = delim "\"" "\""

interleave :: (Pretty a, Pretty b) => b -> [a] -> PrettyP
interleave x xs
  = do env <- ask
       let x' = env `ppshow` x in
           tell $ intercalate x' $ map (ppshow env) xs

(<+>) :: (Pretty a, Pretty b) => a -> b -> PrettyP
a <+> b = pprint a >> pprint b
infixl 3 <+>

str :: String -> PrettyP
str x = quotes (strClr x)

instance Pretty PrettyP where pprint = id
instance Pretty String where pprint = tell
instance Pretty a => Pretty (ZipList a) where
  pprint (ZipList x) = interleave ", " x

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pprint (x, t, y) = x <+> t <+> y
