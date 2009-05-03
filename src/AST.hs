module AST where

type Number = Int

data AST = AsclWord String
    | AsclInt Number
    | AsclFloat Double
    | AsclBool Bool
    | AsclChar Char
    | AsclString String
    deriving (Eq, Show)
