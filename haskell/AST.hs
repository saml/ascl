module AST where

-- | values
-- maybe need Byte
data Value =
    -- | maybe this should be Int..
    Int Integer
    | Float Double
    | Char Char
    | Bool Bool
    -- | something like lisp symbol
    | Symbol String
    -- | function. lambda args statements
    | Fun [String] [Statement]
    deriving (Show, Eq)


-- | expressions
data Expr =
    Value Value
    -- | function call. first Expr should be Fun. [Expr] are args
    | FunCall Expr [Expr]
    deriving (Show, Eq)

-- | statemets
data Statement =
    -- | one expression is a statement ??
    Expr Expr
    -- | binding expr to a name
    | Let String Expr
    deriving (Show, Eq)
