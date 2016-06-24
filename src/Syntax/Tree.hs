module Syntax.Tree where
-- Useful: https://github.com/fsharp/fsharp/blob/master/src/fsharp/ast.fs

-- | A non-qualified name
type Ident = String

-- | A valid name
data Name
  -- | A reference to a variable in a scope
  = ScopeName Ident
  -- | A reference to a fully qualified module member
  | QualifiedName [Ident] Ident
  deriving (Show, Eq, Ord)

data AccessLevel = Public | Private | Internal deriving (Show, Eq, Ord)

type TypeVar = String

-- | All basic types
data Type
  -- | A reference to a type parameter
  = TVar TypeVar
  -- | A reference to a named type
  | TIdent Name
  -- | Instantate a type with a type parameter
  | TInst Type Type
  -- | A function type
  | TFunc Type Type
  -- | A tuple type
  | TTuple [Type]
  -- | A generic type, with a constraint.
  | TForAll { var :: TypeVar, cons :: [Type], td:: Type }
  deriving (Show, Eq, Ord)

data TypeDef
  -- | A tagged union
  = TDUnion [(Ident, Type)]
  -- | A record structure
  | TDRecord [RecordRow]
  -- | An alias for another type
  | TDAlias Type
  deriving (Show, Eq, Ord)

-- | A single row of a record
data RecordRow
  = RecordRow { key :: Ident, ty :: Type, mutable :: Bool, access :: AccessLevel }
  deriving (Show, Eq, Ord)

-- | A basic literal
data Literal
  = LString String
  | LNumber Double
  | LBoolean Bool
  | LUnit
  deriving (Show, Eq, Ord)

-- | The left hand side of a declaration expression
data Declaration
  -- | A single variable
  = DName Ident
  -- | An underscore: discard the variable
  | DDiscard
  -- | A tuple destructuring assignment
  -- This must contain at least two items
  | DTuple [Declaration]
  -- | A record destructuring assignment
  | DRecord [(Name, Declaration)]
  deriving (Show, Eq)

-- | The left hand side of an assignment expression
data Assignable
  -- | A single variable
  = AName Ident
  -- | A tuple destructuring
  | ATuple [Assignable]
  deriving (Show, Eq)

-- | A single variable in a let binding
data LetBinding
  -- TODO: Operators
  = LetBinding { lName :: Declaration, lExpr :: Expr, lMutable :: Bool }
  deriving (Show, Eq)

-- | A basic expression
data Expr
  = ELiteral Literal
  -- | A list of expressions.
  -- This must contain at least two items
  | ETuple [Expr]
  -- | The name of a variable
  | EVar Name
  -- | Indexing an expression (a.b)
  | EIndex Expr Ident
  -- | Ternary expression.
  -- If the first expression succedes then second is evaluated, otherwise the third is
  | EIf Expr Expr Expr
  -- | A function call
  | EApply Expr Expr
  -- | Call a function binary op style.
  -- This exists as evaluation order is different.
  | EBinOp Expr Expr Expr
  -- | Explicitly mark an expression as having a type
  -- This is asserted at compile time
  | EUpcast Expr Type
  -- | Attempt to cast an expression to a type.
  -- This is asserted at run time
  | EDowncast Expr Type
  -- | A lambda definition.
  -- A typed lambda is represented with a lambda then an upcast
  | ELambda Ident (Maybe Type) Expr
  -- | A let binding
  | ELet { recursive :: Bool, vars :: [LetBinding], expr :: Expr }
  -- | Assign an expression to a variable
  | EAssign Assignable Expr Expr
  -- | A list constructor.
  | EList [Expr]
  -- | A pattern match
  | EMatch [(Pattern, Expr)] Expr
  deriving (Show, Eq)

-- | A pattern in a pattern matching expression
data Pattern
  -- | Capture a variable
  = PCapture Ident Pattern
  -- | Matches anything
  | PWildcard
  -- | A constant value
  | PLiteral Literal
  -- | Any of these items
  | POr [Pattern]
  -- | All of these items
  | PAnd [Pattern]
  -- | Matches against a tuple
  | PTuple [Pattern]
  -- | Matches against a list
  | PList [ Pattern ]
  -- | Matches against a named pattern
  | PPattern Name Pattern
  deriving (Show, Eq)

data Import
  = IAll [Ident]
  | INamed [Ident] Ident
  | IPartial [Ident] [(Ident, Ident)]
  deriving (Show, Eq)

data Statement
  = SModule Name AccessLevel [Import] [Statement]
  | STypeDef [(Ident, TypeDef)]
  | SLet { slRecursive :: Bool, slVars ::  [(Ident, Expr)]} -- TODO: Declaration
  deriving (Show, Eq)

data Context
  = CExpr Expr
  | CPattern Pattern
  | CStatement Statement
  | CImport Import
  | CType Type
  deriving (Show, Eq)


treeMatches :: (Expr -> Bool) -> Expr -> Bool
treeMatches p (ELiteral l)    = p $ ELiteral l
treeMatches p (ETuple xs)     = foldr1 (&&) $ map p xs
treeMatches p (EList xs)      = foldr1 (&&) $ map p xs
treeMatches p (EIndex x _)    = p x
treeMatches p (EApply x y)    = p x && p y
treeMatches p (EIf c t e)     = p c && p t && p e
treeMatches p (EBinOp l o r)  = p l && p o && p r
treeMatches p (ELambda _ _ e) = p e
treeMatches p (ELet _ _ b)    = p b
treeMatches p (EAssign _ x y) = p x && p y
treeMatches p (EMatch ps e)   = p e && foldr1 (&&) (map (uncurry $ \_ y -> p e) ps)
treeMatches p e               = p e

applyTree :: (Expr -> Expr) -> Expr -> Expr
applyTree p (ETuple expr)    = ETuple $ map p expr
applyTree p (EList expr)     = EList $ map p expr
applyTree p (EIndex e x)     = EIndex (p e) x
applyTree p (EBinOp l o r)   = EBinOp (p l) (p o) (p r)
applyTree p (EIf c t e)      = EIf (p c) (p t) (p e)
applyTree p (EApply x y)     = EApply (p x) (p y)
applyTree p (EDowncast ex t) = EDowncast (p ex) t
applyTree p (EUpcast ex t)   = EUpcast (p ex) t
applyTree p (ELambda x t e)  = ELambda x t $ p e 
applyTree p (ELet rc va e)   = ELet rc (map (\(LetBinding n e m) -> LetBinding n (p e) m) va) (p e)
applyTree p (EMatch ps e)    = EMatch (map (uncurry $ \x y -> (x, p y)) ps) (p e)
applyTree p e = p e
