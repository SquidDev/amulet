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
compileExpr (EIf c t e)
  = L.EApply $ L.Call (L.Function [] False [L.If [(compileExpr c, [compileExprS t])] [compileExprS e]]) []
compileExpr (EApply e x) = L.EApply $ L.Call (compileExpr e) [(compileExpr x)]
compileExpr (EBinOp l o s) = L.EApply $ L.Call (compileExpr o) [compileExpr l, compileExpr s]
compileExpr (ELambda var _ e) = L.Function [var] False $ [compileExprS e]
compileExpr (ELet _ vs e) = L.EApply $ L.Call (compileLet vs e) []
compileExpr (EList es) = L.Table $ compileList es 1
compileExpr (EAssign x' x b) = compileLet [(LetBinding (atd x') x False)] b 
compileExpr (EMatch ps e) = L.EApply $ L.Call (L.Function ["__pattern__"] False [genMatch ps]) [compileExpr e]

genMatch :: [(Pattern, Expr)] -> L.Statement
genMatch [] = L.Return L.Nil
genMatch xs = L.If (map mktif' xs) [L.Return L.Nil]
  where mktif p r = let (x, y) = compilePattern p in case y of 
                        Nothing -> (x, [compileExprS r])
                        Just x' -> (x, x':[compileExprS r])
        mktif' (x, y) = mktif x y


compileLiteral (LString l) = L.String l
compileLiteral (LNumber d) = L.Number d
compileLiteral (LBoolean False) = L.False
compileLiteral (LBoolean True) = L.True

compileList (x:xs) n = (L.Number n, compileExpr x):compileList xs (n + 1)
compileList [] _ = []

compileLet xs e = L.Function [] False [L.Do $ (map mklocal $ filter ndisc xs) ++ [compileExprS e]]
  where mklocal (LetBinding n' e' _) = L.Local [(dtn n', compileExpr e')]
        ndisc (LetBinding DDiscard _ _) = False
        ndisc (LetBinding _ _ _) = True


atd :: Assignable -> Declaration
atd (AName x) = DName x
atd (ATuple x) = DTuple $ map atd x

dtn :: Declaration -> Ident
dtn (DName x) = x
dtn (DDiscard) = "_"
dtn (DTuple xs) = intercalate ", " $ map dtn xs

compilePattern :: Pattern -> (L.Expr, Maybe L.Statement)
compilePattern PWildcard = (L.Name $ L.Scoped "__pattern__", Nothing)
compilePattern (PLiteral l) = (L.Op (L.Name $ L.Scoped "__pattern__") L.Eq $ Just (compileLiteral l), Nothing)
compilePattern (POr pats) = (operatorMany pats L.Or, Nothing)
compilePattern (PAnd pats) = (operatorMany pats L.And, Nothing)
compilePattern (PCapture nm pat) = (fst $ compilePattern pat, Just (L.Set (L.Scoped nm) (L.Name $ L.Scoped "__pattern__")))

operatorMany :: [Pattern] -> L.Operator -> L.Expr
operatorMany [x] _ = fst $ compilePattern x
operatorMany (x:xs) o = L.Op (fst $ compilePattern x) o $ Just (operatorMany xs o)
operatorMany [] _  = L.True

