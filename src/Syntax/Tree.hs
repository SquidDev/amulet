module Tree () where
-- Useful: https://github.com/fsharp/fsharp/blob/master/src/fsharp/ast.fs

-- A non-qualified name
newtype Ident = Ident String deriving (Show, Eq)

-- |A valid name
data Name
  -- |A reference to a single name
  = Name Ident
  -- |A reference to a module's field
  | ModuleName Name Ident
  deriving (Show, Eq)

data AccessLevel = Public | Private | Internal deriving (Show, Eq)

newtype TypeVar =
  TypeVar String
  deriving (Show, Eq)

-- |All basic types
data Type
  -- |The top type
  = TValue
  -- |The bottom type
  | TNothing
  -- |The unit type
  | TUnit
  -- |A reference to a type parameter
  | TVar TypeVar
  -- |A reference to a named type
  | TIdent Name
  -- |A function type
  | TFunc Type Type
  -- |A tuple type
  | TTuple [Type]
  -- |A type constraint
  | TConstraint { var :: TypeVar, cons :: Type, td:: Type }
  deriving (Show, Eq)

data TypeDef
  -- |A tagged union
  = TDUnion [(Ident, Type)]
  -- |A record structure
  | TDRecord [RecordRow]
  -- |An alias for another type
  | TDAlias Type
  deriving (Show, Eq)

-- |A single row of a record
data RecordRow
  = RecordRow { key :: Ident, ty :: Type, mutable :: Bool, access :: AccessLevel }
  deriving (Show, Eq)

-- |A basic literal
data Literal
  = LString String
  | LNumber Double
  | LBoolean Bool
  | LUnit
  deriving (Show, Eq, Ord)

-- |The left hand side of a declaration expression
data Declaration
  -- |A single variable
  = DName Ident
  -- |An underscore: discard the variable
  | DDiscard
  -- |A tuple destructuring assignment
  -- This must contain at least two items
  | DTuple [Declaration]
  -- |A record destructuring assignment
  | DRecord [(Name, Declaration)]
  deriving (Show, Eq)

-- |The left hand side of an assignment expression
data Assignable
  -- |A single variable
  = AName Ident
  -- |A tuple destructuring
  | ATuple [Assignable]
  deriving (Show, Eq)

-- |A single variable in a let binding
data LetBinding
  -- TODO: Operators
  = LetBinding { lName :: Declaration, lExpr :: Expr, lMutable :: Bool }
  deriving (Show, Eq)

-- |A basic expression
data Expr
  = ELiteral Literal
  -- |A list of expressions.
  -- This must contain at least two items
  | ETuple [Expr]
  -- |The name of a variable
  | EVar Ident
  -- |Indexing an expression (a.b)
  | EIndex Expr Ident
  -- |Ternary expression.
  -- If the first expression succedes then second is evaluated, otherwise the third is
  | EIf Expr Expr Expr
  -- |A function call
  | EApply Expr Expr
  -- |Call a function binary op style.
  -- This exists as evaluation order is different.
  | EBinOp Expr Expr Expr
  -- |Explicitly mark an expression as having a type
  -- This is asserted at compile time
  | EUpcast Expr Type
  -- |Attempt to cast an expression to a type.
  -- This is asserted at run time
  | EDowncast Expr Type
  -- |A lambda definition.
  -- A typed lambda is represented with a lambda then an upcast
  | ELambda Name Expr
  -- |A let binding
  | ELet { recursive :: Bool, vars :: [LetBinding], expr :: Expr }
  -- |Assign an expression to a variable
  | EAssign Assignable Expr Expr
  -- |A list constructor.
  | EList [Expr]
  deriving (Show, Eq)

data Import
  = IAll Name
  | INamed Name Ident
  | IPartial Name [(Ident, Ident)]
  deriving (Show, Eq)

data Statement
  = SModule String AccessLevel [Import] [Statement]
  | STypeDef [(Name, TypeDef)]
  | SExpr Expr
  deriving (Show, Eq)