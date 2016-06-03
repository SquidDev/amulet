module Infer.Expr (
  infer,
  typeNum, typeBool, typeStr, typeUnit, typeList
) where

import Syntax.Tree
import Infer
import Control.Monad.RWS

import qualified Data.Map as Map

typeNum :: Type
typeNum = TIdent $ QualifiedName ["Amulet"] "Num"

typeBool :: Type
typeBool = TIdent $ QualifiedName ["Amulet"] "Bool"

typeStr :: Type
typeStr = TIdent $ QualifiedName ["Amulet"] "Str"

typeUnit :: Type
typeUnit = TIdent $ QualifiedName ["Amulet"] "Unit"

typeList :: Type
typeList = TIdent $ QualifiedName ["Amulet"] "List"

infer :: Expr -> Infer Type
infer (ELiteral (LNumber _)) = return typeNum
infer (ELiteral (LBoolean _)) = return typeBool
infer (ELiteral (LString _)) = return typeStr
infer (ELiteral LUnit) = return typeUnit

infer (EVar x) = lookupEnv x

infer (ELambda x ty e) = do
  tv <- case ty of
          Just ty -> return ty
          Nothing -> fresh
  t <- inEnv (TypeEnv $ Map.singleton (ScopeName x) tv) $ infer e
  return $ tv `TFunc` t

infer e@(EApply e1 e2) = do
  tfun <- infer e1
  targ <- infer e2
  tret <- fresh
  uni e tfun $ targ `TFunc` tret
  return tret

infer e@ELet { recursive = recur, vars = vars, expr = rest } = do
  env <- ask
  -- Build a list of new types and bindings
  (varTys, varEnvs) <- mapAndUnzipM inferDeclr $ map lName vars
  -- Merge all bindings togther
  let varEnv = foldl envUnion nullEnv varEnvs
  -- When evaluated this will unify bindings and types
  let exprs = mapM_ merge $ zip varTys (map lExpr vars)

  if recur then
    -- Declare variables then unify bindings
    inEnv varEnv (exprs >> infer rest)
  else do
    -- Unify bindings then declare variables
    exprs
    inEnv varEnv $ infer rest
  where
    -- Infers a type and unifies it with an expression
    merge (ty, expr) = infer expr >>= uni e ty

infer e@(EIf cond tr fl) = do
  tcond <- infer cond
  ttr <- infer tr
  tfl <- infer fl
  uni e tcond typeBool
  uni e ttr tfl
  return ttr

infer e@(EUpcast expr ty) = do
  t1 <- infer expr
  uni e t1 ty
  return ty

infer (EDowncast expr ty) = do
  _ <- infer expr
  return ty

infer (ETuple exprs) = TTuple <$> mapM infer exprs

infer e@(EList exprs) = do
  tys <- mapM infer exprs
  ty <- case tys of
    [] -> fresh
    [ty] -> return ty
    ty:tys -> do
      mapM_ (uni e ty) tys
      return ty
  return $ TInst typeList ty

infer e@(EBinOp l op r) = do
  tl <- infer l
  top <- infer op
  tr <- infer r

  tret <- fresh

  uni e top $ TFunc tl $ TFunc tr tret
  return tret

-- | Gather the types of a declaration
-- This doesn't unify types, but simply builds an environment from variable names
inferDeclr :: Declaration -> Infer (Type, TypeEnv)
inferDeclr DDiscard = fresh >>= \var -> return (var, nullEnv)
inferDeclr (DName name) = fresh >>= \var -> return (var, TypeEnv $ Map.singleton (ScopeName name) var)
inferDeclr (DTuple items) = do
  fetched <- mapM inferDeclr items
  let tys = map fst fetched
  let env = foldl union nullEnv (map snd fetched)
  return (TTuple tys, env)
  where union (TypeEnv l) (TypeEnv r) = TypeEnv $ Map.union l r
