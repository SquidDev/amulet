module Amulet.AST where

import Amulet.Core (Name, Literal)

type AVar = Name

data Position = Position String Int Int

data Assoc = AssLeft | AssRight | AssNone
type OpDef = (Int, Assoc)

data AExpr = AELiteral Literal
           | AEVar AVar
           | AEAnnot AExpr AType
           | AEApply AExpr AExpr
           | AELambda AVar (Maybe AType) AExpr
           | AEVector [AExpr]
           | AETuple [AExpr]
           | AEMatch [(APattern, AExpr)]
           | AELet [(APattern, AExpr)] AExpr -- TODO: Operator bindings
           | AELetRec [(APattern, AExpr)] AExpr
           | AEPositioned AExpr Position

data AType = ATVar AVar
           | ATApply AType AType
           | ATForAll AVar (Maybe AType) AType
           | ATConstraint AType AType
           | ATFunc AType AType
           | ATTuple [AType]
           | ATRow [(String, AType)] [AVar]
           | ATUnion [AType] [AVar]
           | ATPoisition AType Position

data APattern = APVar AVar
              | APLiteral Literal
              | APDiscard
              | APBind AVar APattern
              | APCtor AVar [APattern]
              | APVector [APattern]
              | APTuple [APattern]
              | APRecord [(String, APattern)] APattern
              | APPositioned APattern Position
