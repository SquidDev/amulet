module Codegen.Optimize where

import Codegen.Lua hiding (True, False)
import qualified Codegen.Lua as L

import Control.Arrow
import Data.Function

import Debug.Trace

debug a x = trace x a

optimize :: Expr -> Expr
optimize x = x
             & applyOpt unwrapSimple
             & applyOpt constFoldSimple

unwrapSimple :: Expr -> Expr
unwrapSimple (Function p d [Do xs]) = Function p d xs
unwrapSimple (Tuple [x]) = x
unwrapSimple e = e

constFoldSimple :: Expr -> Expr
constFoldSimple (Op (Number x) o (Just (Number y)))
  | mathOp o  = performMaths x o y
  | otherwise = Op (Number x) o $ Just $ Number y

constFoldSimple (EApply (Call fn pa)) = foldFunction fn pa
constFoldSimple e = e


foldFunction :: Expr -> [Expr] -> Expr
foldFunction (Function [] False [Return x]) [] = optimize x
foldFunction f@(Function [x] False [Return (Name (Scoped y))]) [e]
  | x == y = e
  | otherwise = f

foldFunction f@(Function [x] False [Return (Table xs)]) [e]
  | noDeps x xs e = Table $ replaceWith e xs
  | otherwise = f
foldFunction (Function arg dots xs) e = EApply $ Call (Function arg dots $ foldBody xs e) e
foldFunction e x = EApply $ Call e x

noDeps :: String -> [(Expr, Expr)] -> Expr -> Bool
noDeps x xs y = all (\(i, v) -> constantIn x i && constantIn x v) xs
  where constantIn x (Name (Scoped y)) = x == y
        constantIn _ (String x) = True
        constantIn _ (Number x) = True
        constantIn _ L.True = True
        constantIn _ L.False = True
        constantIn _ L.Dots = True

replaceWith :: Expr -> [(Expr, Expr)] -> [(Expr, Expr)]
replaceWith x = map (second $ const x)


foldBody :: [Statement] -> [Expr] -> [Statement]
foldBody e@[Local [(x, v)], Return (Name (Scoped y))] _
  | x == y = [Return $ optimize v]
  | otherwise = e
foldBody e _ = e


mathOp :: Operator -> Bool
mathOp Add = True
mathOp Sub = True
mathOp Mul = True
mathOp Div = True

performMaths :: Double -> Operator -> Double -> Expr
performMaths x o y = Number $ performMaths' x o y
  where performMaths' x Add y = x + y
        performMaths' x Sub y = x - y
        performMaths' x Mul y = x * y
        performMaths' x Div y = x / y

applyOpt :: (Expr -> Expr) -> Expr -> Expr
applyOpt x (Function a d b) = x $ Function a d $ optimizeTree b
applyOpt o (Table prs) = o $ Table (map (optimize *** optimize) prs)
applyOpt o (EApply a) = o $ EApply $ optimizeApp a
applyOpt o (Op l x (Just r)) = o $ Op (optimize l) x $ Just (optimize r)
applyOpt o (Op l x Nothing) = o $ Op (optimize l) x Nothing
applyOpt o (Tuple xs) = o $ Tuple $ map optimize xs
applyOpt o (Index e s) = o $ Index (optimize e) s
applyOpt _ e = e

optimizeTree = map optimizeTree'

optimizeTree' :: Statement -> Statement
optimizeTree' (Do xs) = Do $ optimizeTree xs
optimizeTree' (Set n v) = Set n $ optimize v
optimizeTree' (While c b) = While (optimize c) $ optimizeTree b
optimizeTree' (Repeat c b) = Repeat (optimize c) $ optimizeTree b
optimizeTree' (If cnds e) = If (map (optimize *** optimizeTree) cnds) (optimizeTree e)
optimizeTree' (ForIn it b) = ForIn (map (second optimize) it) b
optimizeTree' (ForRange s e st i b) = ForRange s e st i $ optimizeTree b
optimizeTree' (SApply a) = SApply $ optimizeApp a
optimizeTree' (Return e) = Return $ optimize e
optimizeTree' (Local nms) = Local $ map (second optimize) nms

optimizeApp :: Application -> Application
optimizeApp (Call x y) = Call (optimize x) (map optimize y)
optimizeApp (Invoke x y z) = Invoke x y $ map optimize z

