module Codegen.Codegen where

import qualified Codegen.Lua as L
import qualified Data.Map as M
import Syntax.Tree

import Data.List

import Analysis.SymbolTable

compile :: SymbolTable -> [L.Statement]
compile = map compileSym . M.toAscList

compileSym :: SymbolNamed -> L.Statement
compileSym = compileSym' 
  where compileSym' (nm, (SymbolLet _ ex)) = L.Set (L.Scoped nm) $ compileExpr ex

compileExprS = L.Return . compileExpr

compileExpr (ELiteral l) = compileLiteral l
compileExpr (ETuple e) = L.Tuple $ map compileExpr e
compileExpr (EVar (ScopeName x)) = L.Name $ L.Scoped x
compileExpr (EVar (QualifiedName xs x)) = L.Name $ L.Qualified xs x
compileExpr (EIndex e n) = L.Index (compileExpr e) n
compileExpr (EIf c t e) = L.Function [] False [L.If [(compileExpr c, [compileExprS t])] [compileExprS e]]
compileExpr (EApply e x) = L.EApply $ L.Call (compileExpr e) [(compileExpr x)]
compileExpr (EBinOp l o s) = L.EApply $ L.Call (compileExpr o) [compileExpr l, compileExpr s]
compileExpr (ELambda var _ e) = L.Function [var] False $ [compileExprS e]
compileExpr (ELet _ vs e) = compileLet vs e
compileExpr (EList es) = L.Table $ compileList es 1
compileExpr (EAssign x' x b) = compileLet [(LetBinding (atd x') x False)] b 

compileLiteral (LString l) = L.String l
compileLiteral (LNumber d) = L.Number d
compileLiteral (LBoolean False) = L.False
compileLiteral (LBoolean True) = L.True

compileList (x:xs) n = (L.Number n, compileExpr x):compileList xs (n + 1)
compileList [] _ = []

compileLet ((LetBinding DDiscard _ _):xs) e = compileLet xs e
compileLet ((LetBinding n' e' _):xs) e
  = L.EApply $ L.Call (L.Function [(dtn n')] False [L.Return $ compileLet xs e]) [compileExpr e']
compileLet [] e = compileExpr e

atd :: Assignable -> Declaration
atd (AName x) = DName x
atd (ATuple x) = DTuple $ map atd x

dtn :: Declaration -> Ident
dtn (DName x) = x
dtn (DDiscard) = "_"
dtn (DTuple xs) = intercalate ", " $ map dtn xs
