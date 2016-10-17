module Amulet.Core where

import Amulet.Pretty

import qualified Data.Map as Map
import qualified Data.Set as Set

data Name = Name Int String

instance Pretty Name where
  pprint (Name id nm) = nm <+> greyOut ("#" <+> id)

data Var = TermVar Name Type
         | TypeVar Name Type
         | CoercionVar Name Type Type

instance Pretty Var where
  pprint (TermVar n t) = parens $ n <+> opClr " : " <+> t
  pprint (TypeVar n t) = parens $ tvClr n <+> opClr " : " <+> t
  pprint (CoercionVar n f t) = parens $ tvClr n <+> opClr " : " <+> f <+> opClr " ~ " <+> t

data Expr = EVar Var
          | ELiteral Literal
          | ELet Var Expr Expr
          | ELetRec [(Var, Expr)] Expr
          | ELambda Var Expr
          | EApply Expr Expr
          | EMatch Expr [(Pattern, Expr, Expr)]

instance Pretty Expr where
  pprint (EVar v) = pprint v
  pprint (ELiteral l) = pprint l
  pprint (ELet v e b) = kwClr "let " <+> v <+> " = " <+> e <+> kwClr " in " <+> b
  pprint (ELetRec vs b) = kwClr "let " <+> interleave (kwClr " and ") vs' <+> kwClr " in " <+> b where
    vs' = map (\(x, y) -> (x, y, opClr " = ")) vs

  pprint (ELambda v e) = kwClr "λ" <+> v <+> ". " <+> e
  pprint (EApply f@ELambda{} e@ELambda{}) = parens f <+> " " <+> parens e
  pprint (EApply f@ELambda{}           e) = parens f <+> " " <+> e
  pprint (EApply           f           e) = f <+> " " <+> e

data Type = TVar Var
          | TName Var
          | TType
          | TApply Type Type
          | TForAll Var Type
          | TConstraint Type Type
          | TFunc Type Type
          | TRow (Map.Map String Type) (Set.Set Var)
          | TUnion (Set.Set Type) (Set.Set Var)

instance Pretty Type where
  pprint (TVar v) = pprint v
  pprint (TName v) = pprint v
  pprint (TApply f e) = f <+> " " <+> e
  pprint (TForAll v t) = kwClr "∀ " <+> v <+> opClr ". " <+> t
  pprint (TConstraint c t) = c <+> opClr " => " <+> t
  pprint (TFunc f t) = f <+> opClr " -> " <+> t
  pprint TType = typeClr "Type"
  pprint (TRow fields union) =
    let fields' = interleave ", " $ map (\(n, t) -> pprint n <+> " : " <+> pprint t) $ Map.toAscList fields in
    let unions' = interleave " | " $ map pprint $ Set.toAscList union in
    parens (fields' <+> " | " <+> unions')
  pprint (TUnion types union) =
    let types' = interleave ", " $ map pprint $ Set.toAscList types in
    let unions' = interleave " | " $ map pprint $ Set.toAscList union in
    braces (types' <+> " | " <+> unions')

data Literal = LString String
             | LInt Integer
             | LFloat Double

instance Pretty Literal where
  pprint (LString s) = str s
  pprint (LInt i) = litClr i
  pprint (LFloat d) = litClr d

data Pattern = PLiteral Literal
             | PWildcard
             | PTuple [Pattern]
             | PBind Var Pattern
             | PConstructor Var [Pattern]
             | PVector [Pattern]
             | PRecord [(String, Pattern)] Pattern
