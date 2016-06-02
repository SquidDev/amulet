module Builders
  (
    evar, forAll, lambda,
    lNum, lStr,
    mismatch
  ) where

import Test
import Syntax.Tree
import Pretty

evar = EVar . ScopeName
forAll name = TForAll name []
lambda var = ELambda var Nothing

lNum = ELiteral . LNumber
lStr = ELiteral . LString

mismatch :: Pretty a => Pretty b => a -> b -> Result
mismatch expected got = Fail $ "Expected " ++ pshow expected ++ "\n     got " ++ pshow got
