module Analysis.SymbolTable where

import Syntax.Tree
import qualified Data.Map as M

import Data.List

data Symbol
  = SymbolLet Bool Expr
  deriving (Eq, Show)

type SymbolNamed = (String, Symbol)
type SymbolTable = M.Map String Symbol

stToSyms :: Statement -> [SymbolNamed]
stToSyms (SLet rc sl) = map (letsym rc) sl
  where letsym rec (id, st) = (id, SymbolLet rec st)
stToSyms _ = []

erase :: [Statement] -> [Statement]
erase = filter erase'
  where erase' e@(SLet _ _) = True
        erase' _ = False

mkST :: [Statement] -> M.Map String Symbol
mkST = M.fromAscList . concat . map stToSyms . erase
