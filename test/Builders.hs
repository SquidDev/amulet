module Builders
  (
    evar, forAll, lambda,
    lNum, lStr,
    mismatch
  ) where

import Test
import Syntax.Tree
import PrettyPrint

evar = EVar . ScopeName
forAll name ty = TForAll { var = name, cons = [], td = ty }
lambda var ty = ELambda var Nothing ty

lNum = ELiteral . LNumber
lStr = ELiteral . LString

mismatch :: Pretty a => Pretty b => a -> b -> Result
mismatch expected got = Fail $ "Expected " ++ pshow expected ++ "\n     got " ++ pshow got
