module Infer.Run (inferExpr) where

import Infer
import Infer.Expr
import Syntax.Tree

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Identity

import qualified Data.Map as Map

inferExpr :: TypeEnv -> Expr -> Either TypeError Type
inferExpr env ex = do
  (ty, cs) <- runInfer env $ infer ex
  subst <- runSolve cs
  return $ generalize nullEnv $ apply subst ty

runInfer :: TypeEnv -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver (nullSubst, cs)

inferModule :: [String] -> TypeEnv -> Statement -> Either TypeError TypeEnv
inferModule name env (SModule mod _ _ stmt) = foldM (inferModule $ name ++ [mod]) env stmt

inferTop :: TypeEnv -> [(Name, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env@(TypeEnv e) ((name, ex):xs) = do
  ty <- inferExpr env ex
  inferTop (TypeEnv $ Map.insert name ty e) xs
