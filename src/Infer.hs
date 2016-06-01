{-# LANGUAGE FlexibleInstances #-} -- Required for instance of type aliases
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Required for deriving moniod

module Infer where

import Syntax.Tree

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Identity

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Lookup for type names
newtype TypeEnv = TypeEnv (Map.Map Name Type)

-- | Inference moand
type Infer a = (RWST TypeEnv [Constraint] InferState (Except TypeError) a)

data InferState = InferState { count :: Int }

initInfer :: InferState
initInfer = InferState 0

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solve monad
type Solve a = Except TypeError a

newtype Subst = Subst (Map.Map TypeVar Type) deriving (Eq, Ord, Show, Monoid)

data TypeError
  = UnificationFail Type Type
  | InfiniteType TypeVar Type
  | UnboundVariable String
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  deriving (Show)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TypeVar

instance Substitutable Type where
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply _ (TIdent name) = TIdent name -- Should we substitute inside?
  apply (Subst s) (TInst TForAll { var = var, cons = _, td = ty } replace) =
    apply (Subst $ Map.insert var replace s) ty
  apply s (TInst ty rep) = apply s ty `TInst` apply s rep
  apply s (TFunc a r) = apply s a `TFunc` apply s r
  apply s (TTuple items) = TTuple $ map (apply s) items
  apply s TForAll {var = var, cons = cons, td = td} =
    TForAll { var = var, cons = map (apply s) cons, td = apply s td }

  ftv (TIdent _) = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (TInst ty rep) = ftv ty `Set.union` ftv rep
  ftv (TFunc a r) = ftv a `Set.union` ftv r
  ftv (TTuple items) = Set.unions $ map ftv items
  ftv TForAll { var = var, cons = cons, td = td} =
    Set.delete var $ Set.unions $ ftv td : map ftv cons

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

-- | Sream of unique letters
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s { count = count s + 1 }
  return $ TVar $ letters !! count s

occursCheck :: Substitutable a => TypeVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply $ Subst s1) s2 `Map.union` s2

nullSubst :: Subst
nullSubst = Subst Map.empty

nullEnv :: TypeEnv
nullEnv = TypeEnv Map.empty

nullState :: InferState
nullState = InferState { count = 0 }

unify :: Type -> Type -> Infer Subst
unify (l `TFunc` r) (l' `TFunc` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t

unify t1@(TIdent a) t2@(TIdent b)
  | a == b = return nullSubst
  | otherwise = throwError $ UnificationFail t1 t2

unify t1@(TTuple a) t2@(TTuple b)
  | a == b = return nullSubst
  | length a /= length b = throwError $ UnificationFail t1 t2
  | otherwise = foldM folder nullSubst $ zip a b
    where folder :: Subst -> (Type, Type) -> Infer Subst
          folder s (t1, t2) = unify (apply s t1) (apply s t2)

bind :: TypeVar -> Type -> Infer Subst
bind a t | t == TVar a = return nullSubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return $ Subst $ Map.singleton a t

instantiate :: Type -> Infer Type
instantiate ty = do
  let as = Set.toList $ ftv ty
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s ty

generalize :: TypeEnv -> Type -> Type
generalize env t =
  let items = Set.toList $ ftv t `Set.difference` ftv env in
  foldl (\ty name -> TForAll { var = name, cons = [], td = ty }) t items
