module Analysis.Scope
  (
    ModuleScope, moduleVars, moduleTys, moduleChildren,
    findScope, gatherScope, nullScope
  ) where

import Syntax.Tree

import qualified Data.Map as Map

data ModuleScope = ModuleScope {
  moduleVars :: Map.Map Ident Expr,
  moduleTys :: Map.Map Ident Expr,

  moduleChildren :: Map.Map Ident ModuleScope
}

nullScope :: ModuleScope
nullScope = ModuleScope Map.empty Map.empty Map.empty

findScope :: [Ident] -> ModuleScope -> Maybe ModuleScope
findScope [] mod = Just mod
findScope (name:rest) mod = case Map.lookup name $ moduleChildren mod of
                            Nothing -> Nothing
                            Just x -> findScope rest x

gatherScope :: (ModuleScope -> Map.Map Ident a) -> ModuleScope -> Map.Map Name a
gatherScope gather root = gatherImpl [] root
  where gatherImpl prefix modu =
          let moduName = reverse prefix in
          let items = Map.mapKeysMonotonic (QualifiedName moduName) $ gather modu in
          Map.foldlWithKey (\exist k scp -> exist `Map.union` gatherImpl (k : prefix) scp) items $ moduleChildren modu
