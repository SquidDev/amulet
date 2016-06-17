module Codegen.Codegen where

import qualified Codegen.Lua as L
import Syntax.Tree

import Data.List

import Control.Monad.State

data CompilerState
  = CompilerState { statements :: [L.Statement] }
  deriving (Eq, Show)


emptyState :: CompilerState
emptyState = CompilerState []

type Compiler = State CompilerState ()

compile :: Compiler -> [L.Statement]
compile x = let (_, x') = runState x emptyState in statements x'

call :: L.Application -> Compiler
call x 
  = modify $ \s -> s { statements = statements s ++ [L.SApply x] }

function :: String -> [String] -> [L.Statement] -> Compiler
function _ _ [] = modify id 
function n a b =  
  let { va = last a == "..."
      ; fn = L.Function a va b
      ; n' = L.Set (L.Scoped n) fn } in emit n' 

emit :: L.Statement -> Compiler
emit x = modify $ \s -> s { statements = statements s ++ [x] }

if' :: L.Expr -> [L.Statement] -> [L.Statement] -> Compiler
if' cn tr fs = let { cnd = (cn, tr)
                   ; res = L.If [cnd] fs } in emit res

repeat :: L.Expr -> [L.Statement] -> Compiler
repeat c b = emit $ L.Repeat c b

while :: L.Expr -> [L.Statement] -> Compiler
while c b = emit $ L.While c b

set :: String -> L.Expr -> Compiler
set n e = emit $ L.Set (L.Scoped n) e

num :: Double -> L.Expr
num = L.Number

int :: Int -> L.Expr
int x = L.Number $ fromIntegral x

str :: String -> L.Expr
str = L.String

array :: [L.Expr] -> L.Expr
array x = L.Table $ mapI bld x 1
  where bld :: Int -> L.Expr -> (L.Expr, L.Expr) 
        bld i v = (int i, v)
        mapI :: (Int -> a -> b) -> [a] -> Int -> [b]
        mapI _  []     _ = []
        mapI fn [x]    _ = [fn 1 x]
        mapI fn (x:xs) n = (fn n x) : mapI fn xs (n + 1)

return' :: L.Expr -> Compiler
return' x = emit $ L.Return x

compileLit :: Literal -> L.Expr
compileLit (LBoolean True)  = L.True
compileLit (LBoolean False) = L.False
compileLit (LString s)      = L.String s
compileLit (LNumber n)      = L.Number n
compileLit LUnit            = array []


compileExpr :: Expr -> L.Expr
compileExpr (ELiteral l) = compileLit l
compileExpr (ETuple es)  = array $ map compileExpr es
compileExpr (EList es)  = array $ map compileExpr es
compileExpr (EVar (ScopeName x)) = L.Name $ L.Scoped x
compileExpr (EVar (QualifiedName xs x)) = L.Name $ L.Qualified xs x
compileExpr (EIndex e n) = L.Index (compileExpr e) n
compileExpr (EIf c t f) = L.Function [] False $ [L.If [(c', t')] f']
  where c' = compileExpr c
        t' = [L.Return $ compileExpr t]
        f' = [L.Return $ compileExpr f]

compileExpr (EApply e1 e2) = L.EApply $ L.Call (compileExpr e1) [(compileExpr e2)]
compileExpr (EBinOp l o c) = L.EApply $ L.Call (compileExpr o) [(compileExpr l), (compileExpr c)]
compileExpr (ELambda x _ e) = L.Function [x] False $ [L.Return $ compileExpr e]

