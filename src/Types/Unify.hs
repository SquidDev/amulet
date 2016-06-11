{-# LANGUAGE FlexibleInstances #-} -- Required for instance of type aliases
module Types.Unify where

import Syntax.Tree
import Types.TypeVar

import Control.Monad.Except
import Control.Monad.RWS

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Inference moand
type Infer a = (RWST TypeEnv [Constraint] InferState (Except TypeError) a)
type Constraint = (Type, Type, Context)

type Unifier = (Subst, [Constraint])

-- | Constraint solve monad
type Solve a = Except TypeError a

-- | An error which occurs from unification
data TypeError
  = UnificationFail Type Type
  | InfiniteType TypeVar Type
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  | GenericContext Context TypeError
  | TypeContext Type Type TypeError
  deriving (Show)

instance Substitutable Constraint where
  apply s (t1, t2, expr) = (apply s t1, apply s t2, expr)
  ftv (t1, t2, _) = ftv t1 `Set.union` ftv t2

lookupEnv ::Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Nothing -> error $ "Cannot find variable " ++ show x ++ ". This is an amulet bug: it should have been caught though NamedVariables."
    Just s ->  instantiate s

-- | Bind a type variable to a type
bind :: TypeVar -> Type -> Solve Subst
bind a t | t == TVar a     = return nullSubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ Subst $ Map.singleton a t

-- | Unify two types together
unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return nullSubst
unifies (TVar a) t = bind a t
unifies t (TVar a) = bind a t
-- I'm sorry for the repeated catchError: however I couldn't get it to type check
-- when abstracted into another function.
unifies t1@(TFunc l r) t2@(TFunc l' r') = unifyMany [l, r] [l', r'] `catchError` (throwError . TypeContext t1 t2)
unifies t1@(TInst l r) t2@(TInst l' r') = unifyMany [l, r] [l', r'] `catchError` (throwError . TypeContext t1 t2)
unifies t1@(TTuple a) t2@(TTuple b) = unifyMany a b `catchError` (throwError . TypeContext t1 t2)
unifies (TForAll _ _ t1) t2 = unifies t1 t2  `catchError` (throwError . TypeContext t1 t2)
unifies t1 (TForAll _ _ t2) = unifies t1 t2 `catchError` (throwError . TypeContext t1 t2)
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- | Unify many types together
unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return nullSubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return $ su1 `compose` su2
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

-- | Unify two types
uni :: Context -> Type -> Type -> Infer ()
uni e t1 t2 = tell [(t1, t2, e)]

uniE :: Expr -> Type -> Type -> Infer()
uniE = uni . CExpr
-- | Extend type environment
inEnv :: TypeEnv -> Infer a -> Infer a
inEnv env = local $ envUnion env

envUnion :: TypeEnv -> TypeEnv -> TypeEnv
envUnion (TypeEnv new) (TypeEnv original) = TypeEnv $ Map.union new original

solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2, ctx) : cs0) -> do
        su1 <- unifies t1 t2 `catchError` (throwError . GenericContext ctx)
        solver (su1 `compose` su, apply su1 cs0)
