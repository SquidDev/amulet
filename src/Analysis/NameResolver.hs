module Analysis.NameResolver
  (
    runResolver, resolveExpr, resolveStatement,
    Scope, NameError(..), NameResolver, TypeScope(TypeScope), ExprScope(ExprScope),
    nullTy, nullExpr, nullScope
  ) where

import Syntax.Tree
import Analysis.MonadScope
import Analysis.Scope

import Control.Monad.Writer
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

newtype TypeScope = TypeScope (Map.Map Name Name)
newtype ExprScope = ExprScope (Map.Map Name Name)
type Scope = (TypeScope, ExprScope, ModuleScope)

nullTy :: TypeScope
nullTy = TypeScope Map.empty

nullExpr :: ExprScope
nullExpr = ExprScope Map.empty

nullScope :: Scope
nullScope = (nullTy, nullExpr, nullModule)

data NameError
  = UnknownExpr Name Context
  | UnknownType Name Context
  | UnknownModule [Ident] Context
  deriving (Show)

type NameResolver a = WriterT [NameError] (State Scope) a

extendLocal :: Scope -> NameResolver a -> NameResolver a
extendLocal (TypeScope t, ExprScope s, m) a =
  let apply (TypeScope t', ExprScope s', m') = (TypeScope $ Map.union t t', ExprScope $ Map.union s s', mergeModule m m') in
    localScope apply a

extendGlobal :: Scope -> NameResolver ()
extendGlobal (TypeScope t, ExprScope s, m) = do
  ~(TypeScope t', ExprScope s', m') <- getScope
  putScope (TypeScope $ Map.union t t', ExprScope $ Map.union s s', mergeModule m m')

lookupVar :: Name -> Map.Map Name Name -> Maybe Name
lookupVar name@(ScopeName ident) scp =
  case Map.lookup name scp of
    Nothing -> Map.lookup (QualifiedName [] ident) scp
    x@(Just _) -> x
lookupVar name scp = Map.lookup name scp

resolveIdent :: (a -> NameResolver a) -> (Ident, a) -> NameResolver (Ident, a)
resolveIdent func (name, a) = (\a' -> (name, a')) <$> func a

resolveType :: Type -> NameResolver Type
resolveType t@(TVar _) = return t
resolveType t@(TIdent name) = do
  ~(TypeScope scp, _, _) <- getScope
  case lookupVar name scp of
    Nothing -> do
      tell [UnknownType name $ CType t]
      return t
    Just x -> return $ TIdent x
resolveType (TInst a b) = TInst <$> resolveType a <*> resolveType b
resolveType (TFunc a b) = TFunc <$> resolveType a <*> resolveType b
resolveType (TTuple t) = TTuple <$> mapM resolveType t
resolveType (TForAll v c t) = TForAll v <$> mapM resolveType c <*> resolveType t

resolveTypeDef :: TypeDef -> NameResolver TypeDef
resolveTypeDef (TDAlias t) = TDAlias <$> resolveType t
resolveTypeDef (TDUnion vars) = TDUnion <$> mapM (resolveIdent resolveType) vars

resolveExpr :: Expr -> NameResolver Expr
resolveExpr e@(ELiteral _) = return e
resolveExpr (ETuple t) = ETuple <$> mapM resolveExpr t
resolveExpr e@(EVar name) = do
  ~(_, ExprScope scp, _) <- getScope
  case lookupVar name scp of
    Nothing -> do
      tell [UnknownExpr name $ CExpr e]
      return e
    Just x -> return $ EVar x
resolveExpr (EIndex e name) = (`EIndex` name) <$> resolveExpr e
resolveExpr (EIf cond tb fb) = EIf <$> resolveExpr cond <*> resolveExpr tb <*> resolveExpr fb
resolveExpr (EApply fun expr) = EApply <$> resolveExpr fun <*> resolveExpr expr
resolveExpr (EBinOp l op r) = EBinOp <$> resolveExpr l <*> resolveExpr op <*> resolveExpr r
resolveExpr (EUpcast e ty) = EUpcast <$> resolveExpr e <*> resolveType ty
resolveExpr (ELambda name ty e) =
  let name' = ScopeName name in
  ELambda name ty <$> extendLocal (nullTy, ExprScope $ Map.singleton name' name', nullModule) (resolveExpr e)
resolveExpr (ELet recur vars cont) = do
  let names = Set.map ScopeName $ Set.unions $ map (declarationVars . lName) vars
  let scope = (nullTy, ExprScope $ Map.fromSet id names, nullModule)
  let resolveBinding (LetBinding n e m) = (\e' -> LetBinding n e' m) <$> resolveExpr e
  if recur then
    extendLocal scope (ELet recur <$> mapM resolveBinding vars <*> resolveExpr cont)
  else
    ELet recur <$> mapM resolveBinding vars <*> extendLocal scope (resolveExpr cont)

resolveExpr (EDowncast e ty) = EDowncast <$> resolveExpr e <*> resolveType ty
-- TODO: Check assigns exist
resolveExpr (EAssign a v cont) = EAssign a <$> resolveExpr v <*> resolveExpr cont
resolveExpr (EList e) = EList <$> mapM resolveExpr e
resolveExpr (EMatch ptrns match) = do

  EMatch <$> mapM handleBranch ptrns <*> resolveExpr match
  where handleBranch :: (Pattern, Expr) -> NameResolver (Pattern, Expr)
        handleBranch (ptrn, expr) = do
          (scp, ptrn') <- resolvePattern ptrn
          let scp' = (nullTy, ExprScope $ Map.fromSet id $ Set.map ScopeName scp, nullModule)
          expr' <- extendLocal scp' $ resolveExpr expr
          return (ptrn', expr')

-- TODO: Detect multiple assignments
declarationVars :: Declaration -> Set.Set Ident
declarationVars DDiscard = Set.empty
declarationVars (DName name) = Set.singleton name
declarationVars (DTuple vars) = Set.unions $ map declarationVars vars
-- TODO: Handle name on record rows. Maybe: not sure how to do that
declarationVars (DRecord rows) = Set.unions $ map (declarationVars . snd) rows

-- TODO: Detect [multiple assignments] and [same assignment occurs in all "POr" branches]
resolvePattern :: Pattern -> NameResolver (Set.Set Ident, Pattern)
resolvePattern PWildcard = return (Set.empty, PWildcard)
resolvePattern p@(PLiteral _) = return (Set.empty, p)
resolvePattern (PCapture name child) = do
  (scp, child') <- resolvePattern child
  return (Set.insert name scp, PCapture name child')
resolvePattern (POr ptrns) = do
  (scps, ptrns') <- unzip <$> mapM resolvePattern ptrns
  return (Set.unions scps, POr ptrns')
resolvePattern (PAnd ptrns) = do
  (scps, ptrns') <- unzip <$> mapM resolvePattern ptrns
  return (Set.unions scps, PAnd ptrns')
resolvePattern (PTuple ptrns) = do
  (scps, ptrns') <- unzip <$> mapM resolvePattern ptrns
  return (Set.unions scps, PTuple ptrns')
resolvePattern (PList ptrns) = do
  (scps, ptrns') <- unzip <$> mapM resolvePattern ptrns
  return (Set.unions scps, POr ptrns')
resolvePattern p@(PPattern name ptrn) = do
  ~(_, ExprScope scp, _) <- getScope
  name' <- case lookupVar name scp of
    Nothing -> do
      tell [UnknownExpr name $ CPattern p]
      return name
    Just x -> return x
  (scp', ptrn') <- resolvePattern ptrn
  return (scp', PPattern name' ptrn')

buildImport :: (Set.Set Name -> Map.Map Name Name) -> Import -> [Ident] -> NameResolver ()
buildImport handle i path = do
  ~(TypeScope varScp, ExprScope tyScp, scp) <- getScope
  case findScope path scp of
    Nothing -> do
      tell [UnknownModule path $ CImport i]
      return ()
    Just modu -> do
      let vars = handle $ gatherScope (Map.keysSet . moduleVars) modu
      let tys = handle $ gatherScope (Map.keysSet . moduleTys) modu
      putScope (TypeScope $ Map.union tys tyScp, ExprScope $ Map.union vars varScp, scp)
      return ()

handleImport :: Import -> NameResolver ()
handleImport i@(IAll path) = buildImport handle i path
  where qualName :: Name -> Name
        qualName (QualifiedName modu name) = QualifiedName (path ++ modu) name
        handle = Map.fromSet qualName

handleImport i@(INamed path prefix) = buildImport handle i path
  where qualName :: Name -> Name
        qualName (QualifiedName modu name) = QualifiedName (path ++ modu) name
        preName :: Name -> Name
        preName (QualifiedName modu name) = QualifiedName (prefix : modu) name
        handle = Map.mapKeysMonotonic preName . Map.fromSet qualName

handleImport i@(IPartial path renames) = do
  ~(TypeScope varScp, ExprScope tyScp, scp) <- getScope
  case findScope path scp of
    Nothing -> do
      tell [UnknownModule path $ CImport i]
      return ()
    Just modu -> do
      vars <- handle UnknownExpr $ Map.keysSet $ moduleVars modu
      tys <- handle UnknownType $ Map.keysSet $ moduleTys modu
      putScope (TypeScope $ Map.union tys tyScp, ExprScope $ Map.union vars varScp, scp)
      return ()
  where qualName = QualifiedName path
        rename :: (Name -> Context -> NameError) -> Map.Map Ident a -> (Ident, Ident) -> NameResolver (Map.Map Ident a)
        rename err env (from, to) = case Map.lookup from env of
                               Nothing -> do
                                 tell [err (qualName from) $ CImport i]
                                 return env
                               Just x ->
                                 return $ Map.insert to x $ Map.delete from env
        handle :: (Name -> Context -> NameError) -> Set.Set Ident -> NameResolver (Map.Map Name Name)
        handle err set = do
          env <- foldM (rename err) (Map.fromSet qualName set) renames
          return $ Map.mapKeysMonotonic qualName env

resolveStatement :: Statement -> NameResolver Statement
resolveStatement (SLet recur vars) = do
  let names = Set.map ScopeName $ Set.fromList $ map fst vars
  let scope = (nullTy, ExprScope $ Map.fromSet id names, nullModule)
  if recur then do
    extendGlobal scope
    SLet recur <$> mapM (resolveIdent resolveExpr) vars
  else do
    stmt <- SLet recur <$> mapM (resolveIdent resolveExpr) vars
    extendGlobal scope
    return stmt
resolveStatement (STypeDef tys) = do
  let names = Set.map ScopeName $ Set.fromList $ map fst tys
  let scope = (TypeScope $ Map.fromSet id names, nullExpr, nullModule)
  extendGlobal scope
  STypeDef <$> mapM (resolveIdent resolveTypeDef) tys

runResolver :: Scope -> NameResolver a -> Either [NameError] a
runResolver s a =
  let (a', errors) = evalState (runWriterT a) s in
  case errors of
    [] -> Right a'
    items -> Left items
