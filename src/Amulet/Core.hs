module Amulet.Core where

data Name = Name Int String

data Var = TermVar Name Type
         | TypeVar Name Type
         | CoercionVar Name Type Type

data Expr = EVar Var
          | ELiteral Literal
          | ELet Var Expr Expr
          | ELetRec [(Var, Expr)] Expr
          | ELambda Var Expr
          | EApply Expr Expr
          | EMatch Expr [(Pattern, Expr, Expr)]

data Type = TVar Var
          | TApply Type Type
          | TForAll Var Type
          | TConstraint Type Type
          | TFunc Type Type

data Literal = LString String
             | LInt Integer
             | LFloat Double

data Pattern = PLiteral Literal
             | PWildcard
             | PTuple [Pattern]
             | PBind Var Pattern
             | PConstructor Var [Pattern]
             | PVector [Pattern]
             | PRecord [(String, Pattern)] Pattern
