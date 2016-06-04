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
  = PCapture Ident
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
  = IAll Name
  | INamed Name Ident
  | IPartial Name [(Ident, Ident)]
  deriving (Show, Eq)

data Statement
  = SModule Name AccessLevel [Import] [Statement]
  | STypeDef [(Ident, TypeDef)]
  | SExpr Expr
  deriving (Show, Eq)

data Context
  = CExpr Expr
  | CPattern Pattern
  | CStatement Statement
  | CType Type
  deriving (Show, Eq)
