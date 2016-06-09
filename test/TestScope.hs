module TestScope (scopeTests) where

import Builders
import Test

import Syntax.Tree
import Analysis.Scope
import Pretty

import qualified Data.Map as Map
import Data.Maybe (fromJust)

scope :: ModuleScope
scope = nullScope {
  moduleVars = Map.fromList [
      ("A", ELiteral $ LNumber 2),
      ("B", ELiteral $ LNumber 3)
      ],
  moduleChildren = Map.singleton "Child" $ nullScope {
      moduleVars = Map.fromList [
          ("C", ELiteral $ LNumber 4)
          ],
        moduleChildren = Map.singleton "Children" $ nullScope {
          moduleVars = Map.fromList [
              ("C", ELiteral $ LNumber 5)
              ]
          }
      }
  }

testFindScope :: Test
testFindScope = Test "Find scope" $ return result
  where result = case findScope ["Child"] scope of
          Nothing -> mismatch "<Child scope>" "<Nothing>"
          Just x -> Pass

testGatherScope :: Test
testGatherScope = Test "Gather scope" $ return result
  where result =
          let scp = gatherScope moduleVars scope in
            let expected = Map.fromList [
                  (QualifiedName [] "A", ELiteral $ LNumber 2),
                  (QualifiedName [] "B", ELiteral $ LNumber 3),
                  (QualifiedName ["Child"] "C", ELiteral $ LNumber 4),
                  (QualifiedName ["Child", "Children"] "C", ELiteral $ LNumber 5)
                  ] in assertEquals expected scp

scopeTests :: Test
scopeTests = Group "Scope" [testGatherScope, testFindScope]
