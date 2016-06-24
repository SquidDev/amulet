module Codegen.Lua where

data Statement
  = Do     { block      :: [Statement] }
  | Set    { name       :: Name
           , val        :: Expr }
  | While  { cond       :: Expr
           , block      :: [Statement] }
  | Repeat { cond       :: Expr
           , block      :: [Statement] }
  | If     { conds      :: [(Expr, [Statement])]
           , else'      :: [Statement] }
  | ForIn  { iterators  :: [(String, Expr)]
           , block      :: [Statement] }
  | ForRange { start    :: Double
             , end      :: Double
             , step     :: Double
             , iterator :: String
             , block    :: [Statement] }
  | Local  { names      :: [(String, Expr)] }
  | SApply Application
  | Return Expr
  deriving (Eq, Show)

data Expr
  = Nil | Dots | True | False
  | Number Double | String String
  | Function { args    :: [String]
             , hasDots :: Bool
             , body    :: [Statement] }
  | Table    { pairs   :: [(Expr, Expr)] }
  | EApply Application
  | Op { lhs :: Expr
       , op  :: Operator
       , rhs :: Maybe Expr }
  | Tuple [Expr]
  | Index Expr String
  | Name Name
  deriving (Eq, Show)

data Name
  = Scoped String
  | Qualified [String] String
  deriving (Eq, Show)

data Application
  = Call { callee    :: Expr
         , arguments :: [Expr] }
  | Invoke { table :: String
           , method :: String
           , arguments :: [Expr] }
  deriving (Eq, Show)

data Operator
  = Add | Sub | Mul    | Div
  | Mod | Pow | Concat | Eq
  | Lt  | Le  | And    | Or
  | Not | Len | Shr    | Shl
  deriving (Eq, Show)


makeOp :: Expr -> String -> Maybe Expr -> Expr
makeOp e "+"   x = Op e Add x
makeOp e "-"   x = Op e Sub x
makeOp e "*"   x = Op e Mul x
makeOp e "/"   x = Op e Div x
makeOp e "%"   x = Op e Mod x
makeOp e "^"   x = Op e Pow x
makeOp e ".."  x = Op e Concat x
makeOp e "<"   x = Op e Lt x
makeOp e "<="  x = Op e Le x
makeOp e "and" x = Op e And x
makeOp e "not" _ = Op e Not Nothing
makeOp e "or"  x = Op e Or x
makeOp e "#"   _ = Op e Len Nothing
makeOp e ">>"  x = Op e Shr x
makeOp e "<<"  x = Op e Shl x
makeOp _ x     _ = error $ "Invalid operator " ++ x
