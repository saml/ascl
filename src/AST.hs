module AST where

import Text.ParserCombinators.Parsec.Pos
import Text.PrettyPrint

type Number = Int

data AST = At SourcePos AST
    | AsclWord String
    | AsclInt Number
    | AsclFloat Double
    | AsclBool Bool
    | AsclChar Char
    | AsclString String
    deriving (Eq, Ord, Show)

--instance Show AST where show = render . pp

pp (At _ x) = pp x
pp (AsclWord x) = text x
pp (AsclInt x) = integer (fromIntegral x)
pp (AsclFloat x) = double x
pp (AsclBool x) = text $ show x
pp (AsclChar x) = text $ show x
pp (AsclString x) = text $ show x


