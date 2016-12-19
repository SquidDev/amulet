module Amulet.AST
  ( Type
  ) where

data Name
  = NScoped String
  | NQualified [String] String
  deriving (Eq, Show, Ord)

data Type
  = TName Name
  | TApp Type Type
  | TInfix Type Type Type
  | TForall [TypeVar] Type
  | TConstraint Type Type
  | TArrow Type Type
  | TTuple [Type]
  | TUnifies Type Type
  | THole
  | TList Type
  | TVector Type
  | TRow TypeRow
  | TRecord TypeRow

data TypeRow
  = TypeRow [(String, Type)] (Maybe String)

data TypeVar
  = TVName String
  | TVTyped String Type
