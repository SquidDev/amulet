module Analysis.Infer (
  inferExpr,
) where

import Syntax.Tree
import Types.TypeVar
import Types.Unify
import Types.BuiltIn
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Identity

import qualified Data.Map as Map

inferLiteral :: Literal -> Type
inferLiteral LUnit = typeUnit
inferLiteral (LNumber _) = typeNum
inferLiteral (LBoolean _) = typeBool
inferLiteral (LString _) = typeStr

inferList :: Context -> [Type] -> Infer Type
inferList _ [] = fresh
inferList _ [ty] = return ty
inferList c (ty : tys) = do
  mapM_ (uni c ty) tys
  return ty

infer :: Expr -> Infer Type
infer (ELiteral lit) = return $ inferLiteral lit

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
  uniE e tfun $ targ `TFunc` tret
  return tret

infer e@ELet { recursive = recur, vars = vars, expr = rest } = do
  -- Build a list of new types and bindings
  (varTys, varEnvs) <- mapAndUnzipM inferDeclr $ map lName vars
  -- Merge all bindings togther
  let varEnv = foldl envUnion nullEnv varEnvs
  -- When evaluated this will unify bindings and types
  let exprs env = mapM_ (merge env) $ zip varTys $ map lExpr vars
  if recur then
    -- Declare variables then unify bindings
    inEnv varEnv $ ask >>= exprs >> infer rest
  else do
    -- Unify bindings then declare variables
    ask >>= exprs
    inEnv varEnv $ infer rest
  where
    -- Infers a type and unifies it with an expression
    merge :: TypeEnv -> (Type, Expr) -> Infer ()
    merge env (ty, expr) = generalize env <$> infer expr >>= uniE e ty


infer e@(EIf cond tr fl) = do
  tcond <- infer cond
  ttr <- infer tr
  tfl <- infer fl
  uniE e tcond typeBool
  uniE e ttr tfl
  return ttr

infer e@(EUpcast expr ty) = do
  t1 <- infer expr
  uniE e t1 ty
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
      mapM_ (uniE e ty) tys
      return ty
  return $ TInst typeList ty

infer e@(EBinOp l op r) = do
  tl <- infer l
  top <- infer op
  tr <- infer r

  tret <- fresh

  uniE e top $ TFunc tl $ TFunc tr tret
  return tret

infer e@(EMatch patterns expr) = do
  texpr <- infer expr
  let handle (ptrn, expr) = do
        (ty, TypeEnv vars) <- inferPattern ptrn
        uniE e texpr ty

        env <- ask
        inEnv (TypeEnv vars) $ infer expr
  tys <- mapM handle patterns
  case tys of
    [] -> fresh
    [single] -> return single
    ty:tys -> do
      mapM_ (uniE e ty) tys
      return ty

-- | Gather the types of a declaration
-- This doesn't unify types, but simply builds an environment from variable names
inferDeclr :: Assignment -> Infer (Type, TypeEnv)
inferDeclr ADiscard = fresh >>= \var -> return (var, nullEnv)
inferDeclr (AName name) = fresh >>= \var -> return (var, TypeEnv $ Map.singleton (ScopeName name) var)
inferDeclr (ATuple items) = do
  fetched <- mapM inferDeclr items
  let tys = map fst fetched
  let env = foldl envUnion nullEnv $ map snd fetched
  return (TTuple tys, env)

-- TODO: Pass context to parent node
inferPattern :: Pattern -> Infer (Type, TypeEnv)
inferPattern PWildcard = fresh >>= \var -> return (var, nullEnv)
inferPattern (PCapture name ptrn) = do
  (ty, TypeEnv env) <- inferPattern ptrn
  return (ty, TypeEnv $ Map.insert (ScopeName name) ty env)
inferPattern (PLiteral literal) = return (inferLiteral literal, nullEnv)
inferPattern p@(POr ptrns) = do
  fetched <- mapM inferPattern ptrns
  ty <- inferList (CPattern p) $ map fst fetched
  -- TODO: Check all maps are the same
  let env = foldl envUnion nullEnv $ map snd fetched
  return (ty, env)
inferPattern p@(PAnd ptrns) = do
  fetched <- mapM inferPattern ptrns
  ty <- inferList (CPattern p) $ map fst fetched
  let env = foldl envUnion nullEnv $ map snd fetched
  return (ty, env)
inferPattern (PTuple ptrns) = do
  fetched <- mapM inferPattern ptrns
  let env = foldl envUnion nullEnv $ map snd fetched
  return (TTuple $ map fst fetched, env)
inferPattern p@(PList ptrns) = do
  fetched <- mapM inferPattern ptrns
  ty <- inferList (CPattern p) $ map fst fetched
  let env = foldl envUnion nullEnv $ map snd fetched
  return (TInst typeList ty, env)
inferPattern p@(PPattern name ptrn) = do
  (result, env) <- inferPattern ptrn
  matcher <- lookupEnv name
  ty <- fresh
  uni (CPattern p) matcher (ty `TFunc` result)
  return (ty, env)

inferExpr :: TypeEnv -> Expr -> Either TypeError Type
inferExpr env ex = do
  (ty, cs) <- runInfer env $ infer ex
  subst <- runSolve cs
  return $ generalize nullEnv $ apply subst ty

runInfer :: TypeEnv -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver (nullSubst, cs)

inferModule :: String -> TypeEnv -> Statement -> Either TypeError TypeEnv
inferModule name env (SModule (ScopeName mod) _ _ stmt) = foldM (inferModule $ name ++ mod) env stmt
