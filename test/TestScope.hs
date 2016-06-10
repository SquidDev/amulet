module TestScope (scopeTests) where

import Builders
import Test

import Syntax.Tree
import Analysis.Scope
import Pretty

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)

scope :: ModuleScope
scope = nullModule {
  moduleVars = Map.fromList [
      ("A", ELiteral $ LNumber 2),
      ("B", ELiteral $ LNumber 3)
      ],
  moduleChildren = Map.singleton "Child" $ nullModule {
      moduleVars = Map.fromList [
          ("C", ELiteral $ LNumber 4)
          ],
        moduleChildren = Map.singleton "Children" $ nullModule {
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

name :: [Ident] -> Name
name [] = error "Cannot have empty name"
name items = QualifiedName (init items) $ last items

testGatherScope :: Test
testGatherScope = Test "Gather scope" $ return result
  where result =
          let scp = gatherScope (Map.keysSet . moduleVars) scope in
            let expected = Set.fromList [
                  name ["A"],
                  name ["B"],
                  name ["Child", "C"],
                  name ["Child", "Children", "C"]
                  ] in assertEquals expected scp

scopeTests :: Test
scopeTests = Group "Scope" [testGatherScope, testFindScope]
