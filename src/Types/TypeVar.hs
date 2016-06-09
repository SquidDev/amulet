{-# LANGUAGE FlexibleContexts #-} -- Required for MonadState
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Required for deriving moniod

module Types.TypeVar where

import Syntax.Tree

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

data InferState = InferState { count :: Int } deriving (Show)
-- | The starting inference state
initInfer :: InferState
initInfer = InferState 0

-- | Lookup for type names
newtype TypeEnv = TypeEnv (Map.Map Name Type) deriving (Show)
-- | Map of types to substitute
newtype Subst = Subst (Map.Map TypeVar Type) deriving (Eq, Ord, Show, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TypeVar

instance Substitutable Type where
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply _ (TIdent name) = TIdent name -- Should we substitute inside?
  apply (Subst s) (TInst (TForAll var _ ty) replace) =
    apply (Subst $ Map.insert var replace s) ty
  apply s (TInst ty rep) = apply s ty `TInst` apply s rep
  apply s (TFunc a r) = apply s a `TFunc` apply s r
  apply s (TTuple items) = TTuple $ map (apply s) items
  apply s@(Subst m) (TForAll var cons td) = case Map.lookup var m of
                                                                  Just (TVar r)  -> TForAll{ var = r, cons = cons, td = apply s td }
                                                                  Just _ -> apply s td
                                                                  Nothing -> TForAll { var = var, cons = map (apply s) cons, td = apply s td }

  ftv (TIdent _) = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (TInst ty rep) = ftv ty `Set.union` ftv rep
  ftv (TFunc a r) = ftv a `Set.union` ftv r
  ftv (TTuple items) = Set.unions $ map ftv items
  ftv (TForAll var cons td) = Set.delete var $ Set.unions $ ftv td : map ftv cons

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env


tyVars :: Type -> Set.Set TypeVar
tyVars (TVar _) = Set.empty
tyVars (TInst _ _) = Set.empty
tyVars (TIdent _) = Set.empty
tyVars (TForAll var _ ty) = Set.insert var $ tyVars ty
tyVars (TFunc a r) = tyVars a `Set.union` tyVars r
tyVars (TTuple items) = Set.unions $ map tyVars items

nullSubst :: Subst
nullSubst = Subst Map.empty

nullEnv :: TypeEnv
nullEnv = TypeEnv Map.empty

-- | Check a variable occurs in a type
occursCheck :: Substitutable a => TypeVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- | Combine two substitution maps together
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply $ Subst s1) s2 `Map.union` s1

-- | Sream of unique letters
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: MonadState InferState m => m Type
fresh = do
  s <- get
  put s { count = count s + 1 }
  return $ TVar $ letters !! count s

-- | Copy a type using fresh type variables
instantiate :: MonadState InferState m => Type -> m Type
instantiate ty = do
  let var = Set.toList $ tyVars ty
  var' <- mapM (const fresh) var
  let s = Subst $ Map.fromList $ zip var var'
  return $ apply s ty

-- | Create TForAlls for free type variables
generalize :: TypeEnv -> Type -> Type
generalize env t =
  let items = Set.toList $ ftv t `Set.difference` ftv env in
  foldl (\ty name -> TForAll { var = name, cons = [], td = ty }) t items
