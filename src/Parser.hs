module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

import Data.Char (isSpace)

import AST

asclStyle = emptyDef { commentLine = "#" }

asclLexer = makeTokenParser asclStyle

-- | given a number parser, p, parses optional sign +,-
signed p = do
    f <- sign
    x <- p
    return (f x)
    where
        sign = (char '-' >> return negate)
            <|> (char '+' >> return id)
            <|> return id

ws = skipMany1 space >> spaces

asclBool = do
    s <- string "True" <|> string "False"
    case s of
        "True" -> return True
        "False" -> return False

asclInt = signed (natural asclLexer)
asclFloat = signed (float asclLexer)
asclHex = signed (char '0' >> hexadecimal asclLexer)
asclOctal = signed (char '0' >> octal asclLexer)

asclChar = charLiteral asclLexer
asclString = stringLiteral asclLexer

asclWord = manyTill anyChar space

parseBool = asclBool >>= (return . AsclBool)
parseInt = asclInt >>= (return . AsclInt . fromIntegral)
parseFloat = asclFloat >>= (return . AsclFloat)
parseHex = asclHex >>= (return . AsclInt . fromIntegral)
parseOctal = asclOctal >>= (return . AsclInt . fromIntegral)
parseChar = asclChar >>= (return . AsclChar)
parseString = asclString >>= (return . AsclString)
parseWord = asclWord >>= (return . AsclWord)

asclToken = try parseBool
    <|> try parseChar
    <|> try parseString
    <|> try parseHex
    <|> try parseOctal
    <|> try parseFloat
    <|> try parseInt
    <|> try parseWord

asclParser = spaces >> sepEndBy parseInt spaces



