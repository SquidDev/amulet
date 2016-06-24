module Codegen.Optimize where

import Codegen.Lua

optimize :: Expr -> Expr
optimize = applyOpt unwrapSimple

applyOpt :: (Expr -> Expr) -> Expr -> Expr
applyOpt x (Function a d b) = x $ Function a d $ optimizeTree b
applyOpt o (Table prs) = o $ Table (map (\(x, y) -> (optimize x, optimize y)) prs)
applyOpt o (EApply a) = o $ EApply $ optimizeApp a
applyOpt o (Op l x (Just r)) = o $ Op (optimize l) x $ Just (optimize r)
applyOpt o (Op l x Nothing) = o $ Op (optimize l) x Nothing
applyOpt o (Tuple xs) = o $ Tuple $ map optimize xs
applyOpt o (Index e s) = o $ Index (optimize e) s
applyOpt _ e = e

unwrapSimple :: Expr -> Expr
unwrapSimple (Function p d [(Do xs)]) = Function p d xs
unwrapSimple e = e

optimizeTree = map optimizeTree'

optimizeTree' :: Statement -> Statement
optimizeTree' (Do xs) = Do $ optimizeTree xs
optimizeTree' (Set n v) = Set n $ optimize v
optimizeTree' (While c b) = While (optimize c) $ optimizeTree b
optimizeTree' (Repeat c b) = Repeat (optimize c) $ optimizeTree b
optimizeTree' (If cnds e) = If (map (\(x, y) -> (optimize x, optimizeTree y)) cnds) (optimizeTree e)
optimizeTree' (ForIn it b) = ForIn (map (\(x, y) -> (x, optimize y)) it) b
optimizeTree' (ForRange s e st i b) = ForRange s e st i $ optimizeTree b
optimizeTree' (SApply a) = SApply $ optimizeApp a
optimizeTree' (Return e) = Return $ optimize e
optimizeTree' (Local nms) = Local $ map (\(x, y) -> (x, optimize y)) nms

optimizeApp :: Application -> Application
optimizeApp (Call x y) = Call (optimize x) (map optimize y)
optimizeApp (Invoke x y z) = Invoke x y $ map optimize z

