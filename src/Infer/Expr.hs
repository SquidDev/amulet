module Infer.Expr (
  infer,
  typeNum, typeBool, typeStr, typeUnit, typeList
) where

import Syntax.Tree
import Infer
import Control.Monad.RWS

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
  t <- inEnv (ScopeName x) tv $ infer e
  return $ tv `TFunc` t

infer (EApply e1 e2) = do
  tfun <- infer e1
  targ <- infer e2
  tret <- fresh
  uni tfun $ targ `TFunc` tret
  return tret

infer ELet { recursive = recur, vars = vars, expr = rest } = do
  env <- ask
  -- TODO: Handle everything
  let var = head vars
  let x = ScopeName $ case lName var of
                        DName ident -> ident
                        _ -> "_"
  let e1 = lExpr var
  t1 <- infer e1
  let sc = generalize env t1
  inEnv x sc $ infer rest

infer (EIf cond tr fl) = do
  tcond <- infer cond
  ttr <- infer tr
  tfl <- infer fl
  uni tcond typeBool
  uni ttr tfl
  return ttr

infer (EUpcast expr ty) = do
  t1 <- infer expr
  uni t1 ty
  return ty

infer (EDowncast expr ty) = do
  _ <- infer expr
  return ty

infer (ETuple exprs) = TTuple <$> mapM infer exprs

infer (EList exprs) = do
  tys <- mapM infer exprs
  ty <- case tys of
    [] -> fresh
    [ty] -> return ty
    ty:tys -> do
      mapM_ (uni ty) tys
      return ty
  return $ TInst typeList ty

infer (EBinOp l op r) = do
  tl <- infer l
  top <- infer op
  tr <- infer r

  tret <- fresh

  uni top $ TFunc tl $ TFunc tr tret
  return tret
