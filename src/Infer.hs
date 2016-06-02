{-# LANGUAGE FlexibleInstances #-} -- Required for instance of type aliases
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Required for deriving moniod

module Infer where

import Syntax.Tree

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Lookup for type names
newtype TypeEnv = TypeEnv (Map.Map Name Type)

-- | Inference moand
type Infer a = (RWST TypeEnv [Constraint] InferState (Except TypeError) a)

-- | Current inference state
-- Used to get a stream of variable names
data InferState = InferState { count :: Int } deriving (Show)

-- | The starting inference state
initInfer :: InferState
initInfer = InferState 0

type Constraint = (Type, Type, Expr)

type Unifier = (Subst, [Constraint])

-- | Constraint solve monad
type Solve a = Except TypeError a

-- | Map of types to substitute
newtype Subst = Subst (Map.Map TypeVar Type) deriving (Eq, Ord, Show, Monoid)

-- | An error which occurs from unification
data TypeError
  = UnificationFail Type Type
  | InfiniteType TypeVar Type
  | UnboundVariable Name
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  | ExprContext Expr TypeError
  | TypeContext Type Type TypeError
  deriving (Show)

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

instance Substitutable Constraint where
  apply s (t1, t2, expr) = (apply s t1, apply s t2, expr)
  ftv (t1, t2, _) = ftv t1 `Set.union` ftv t2

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

-- | Sream of unique letters
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s { count = count s + 1 }
  return $ TVar $ letters !! count s

lookupEnv ::Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s ->  instantiate s

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

-- | Copy a type using fresh type variables
instantiate :: Type -> Infer Type
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

-- | Unify two types
uni :: Expr -> Type -> Type -> Infer ()
uni e t1 t2 = tell [(t1, t2, e)]

-- | Extend type environment
inEnv :: Name -> Type -> Infer a -> Infer a
inEnv name ty m = do
  -- Can just use Map.insert but implicit is better than explicit or something...
  let scope (TypeEnv s) = TypeEnv $ Map.insert name ty $ Map.delete name s
  local scope m

solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2, expr) : cs0) -> do
        su1 <- unifies t1 t2 `catchError` (throwError . ExprContext expr)
        solver (su1 `compose` su, apply su1 cs0)
