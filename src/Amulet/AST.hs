module Amulet.AST where

import Amulet.Core

data AVar = AScoped [String]
          | AVar Var

data AExpr = ALiteral Literal
           | AEVar AVar
           | AEAnnot AExpr AType
           | AEApply AExpr AExpr
           | AELambda AVar (Maybe AType) AExpr
           | AEVector [AExpr]
           | AETuple [AExpr]
           | AEMatch [(APattern, AExpr)]
           | AELet [(APattern, AExpr)] AExpr
           | AELetRec [(APattern, AExpr)] AExpr
           -- Discarded after operator folding
           | AEParens AExpr
           | AEOps [AExpr]

data AType = ATVar AVar
           | ATApply AType AType
           | ATForAll AVar (Maybe AType) AType
           | ATConstraint AType AType
           | ATFunc AType AType
           | ATTuple [AType]
           | ATRow [(String, AType)] [AVar]
           | ATUnion [AType] [AVar]
           -- Discarded after operator folding
           | ATOps [AType]
           | ATParens AType

data APattern = APVar AVar
              | APLiteral Literal
              | APDiscard
              | APBind AVar APattern
              | APCtor AVar [APattern]
              | APVector [APattern]
              | APTuple [APattern]
              | APRecord [(String, APattern)] APattern
              -- Discarded after operator folder
              | APOps [APattern]
              | APParens APattern
