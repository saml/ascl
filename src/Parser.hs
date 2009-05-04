module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

import Data.Char (isSpace)

import AST

asclStyle = emptyDef { commentLine = "#" }

asclLexer = (makeTokenParser asclStyle) {
    lexeme = (\p -> p >>= return)
    }

-- | given a number parser, p, parses optional sign +,-
signed p = do
    f <- sign
    x <- p
    return (f x)
    where
        sign = (char '-' >> return negate)
            <|> (char '+' >> return id)
            <|> return id

boundary p = do
    x <- p
    lookAhead $ eof <|> (space >> return ())
    return x

asclBool = boundary $ do
    s <- string "True" <|> string "False"
    case s of
        "True" -> return True
        "False" -> return False

asclInt = boundary $ signed (natural asclLexer)
asclFloat = boundary $ signed (float asclLexer)
asclHex = boundary $ signed (char '0' >> hexadecimal asclLexer)
asclOctal = boundary $ signed (char '0' >> octal asclLexer)

asclChar = boundary $ charLiteral asclLexer
asclString = boundary $ stringLiteral asclLexer

asclWord = many1 (satisfy (not . isSpace))

at p = do
    pos <- getPosition
    x <- p
    return $ At pos x

parseBool = at $ AsclBool `fmap` asclBool
parseInt = at $ (AsclInt . fromIntegral) `fmap` asclInt
parseFloat = at $ AsclFloat `fmap` asclFloat
parseHex = at $ (AsclInt . fromIntegral) `fmap` asclHex
parseOctal = at $ (AsclInt . fromIntegral) `fmap` asclOctal
parseChar = at $ AsclChar `fmap` asclChar
parseString = at $ AsclString `fmap` asclString
parseWord = at $ AsclWord `fmap` asclWord

asclToken = try parseBool
    <|> try parseChar
    <|> try parseString
    <|> try parseHex
    <|> try parseOctal
    <|> try parseFloat
    <|> try parseInt
    <|> try parseWord

{-
asclParser = do
    spaces
    x <- sepBy asclToken spaces
    spaces
    eof
    return x
-}

asclParser = spaces >> sepEndBy asclToken (skipMany1 space)


