module Analysis.Scope
  (
    ModuleScope, moduleVars, moduleTys, moduleChildren,
    findScope, gatherScope, nullScope
  ) where

import Syntax.Tree

import qualified Data.Map as Map
import qualified Data.Set as Set

data ModuleScope = ModuleScope {
  moduleVars :: Map.Map Ident Expr,
  moduleTys :: Map.Map Ident Expr,

  moduleChildren :: Map.Map Ident ModuleScope
}

nullScope :: ModuleScope
nullScope = ModuleScope Map.empty Map.empty Map.empty

-- | Find a child module within a scope
findScope :: [Ident] -> ModuleScope -> Maybe ModuleScope
findScope [] mod = Just mod
findScope (name:rest) mod = case Map.lookup name $ moduleChildren mod of
                            Nothing -> Nothing
                            Just x -> findScope rest x

-- | Find a list of all scope elements
gatherScope :: (ModuleScope -> Set.Set Ident) -> ModuleScope -> Set.Set Name
gatherScope gather = gatherImpl []
  where gatherImpl prefix modu =
          let moduName = reverse prefix in
          let items = Set.map (QualifiedName moduName) $ gather modu in
          Map.foldlWithKey (\exist k scp -> exist `Set.union` gatherImpl (k : prefix) scp) items $ moduleChildren modu
