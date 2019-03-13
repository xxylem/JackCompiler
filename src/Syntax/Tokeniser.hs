{-# LANGUAGE OverloadedStrings #-}

module Syntax.Tokeniser where

import Data.Model

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS


import Prelude hiding (takeWhile)

take1Space :: Parser ()
take1Space =    
        (
            (   
                char ' '
            <|> char '\t'
            )
        >> return ()
        )
    <|> endOfLine
    <|> endOfInput

parseKeyword :: Parser Keyword
parseKeyword =
    (
        (string "class"         >> return Class)
    <|> (string "constructor"   >> return Constructor)
    <|> (string "function"      >> return Function)
    <|> (string "method"        >> return Method)
    <|> (string "field"         >> return Field)
    <|> (string "static"        >> return Static)
    <|> (string "var"           >> return Var)
    <|> (string "int"           >> return Int)
    <|> (string "char"          >> return Char)
    <|> (string "boolean"       >> return Boolean)
    <|> (string "void"          >> return Void)
    <|> (string "true"          >> return TrueKW)
    <|> (string "false"         >> return FalseKW)
    <|> (string "null"          >> return Null)
    <|> (string "this"          >> return This)
    <|> (string "let"           >> return Let)
    <|> (string "do"            >> return Do)
    <|> (string "if"            >> return If)
    <|> (string "else"          >> return Else)
    <|> (string "while"         >> return While)
    <|> (string "return"        >> return Return)
    )
    <* take1Space


parseSymbol :: Parser Symbol
parseSymbol =
        (char '{' >> return LCurlyBracket)
    <|> (char '}' >> return RCurlyBracket)
    <|> (char '(' >> return LParen)
    <|> (char ')' >> return RParen)
    <|> (char '[' >> return LSquareBracket)
    <|> (char ']' >> return RSquareBracket)
    <|> (char '.' >> return FullStop)
    <|> (char ',' >> return Comma)
    <|> (char ';' >> return Semicolon)
    <|> (char '+' >> return Plus)
    <|> (char '-' >> return Minus)
    <|> (char '*' >> return Asterix)
    <|> (char '/' >> return Slash)
    <|> (char '&' >> return Ampersand)
    <|> (char '|' >> return VerticalBar)
    <|> (char '<' >> return LAngleBracket)
    <|> (char '>' >> return RAngleBracket)
    <|> (char '=' >> return Equal)
    <|> (char '~' >> return Tilde)

parseIntegerConstant :: Parser IntegerConstant
parseIntegerConstant = decimal

parseStringConstant :: Parser StringConstant
parseStringConstant =
        char '\"'
    >>  takeWhile1 (notInClass "\"\n")
    <*  char '\"'

parseIdentifier :: Parser Identifier
parseIdentifier = do
    firstLetter <-  satisfy (inClass "a-zA-Z_")
    rest        <-  takeWhile (inClass "0-9a-zA-Z_")
    return (BS.cons firstLetter rest)


skipLineComment :: Parser ()
skipLineComment =
        string "//"
    *>  manyTill anyChar (char '\n')
    *>  return ()

skipAPIComment :: Parser ()
skipAPIComment =
        string "/**"
    *>  manyTill anyChar (string "*/")
    *>  return ()

skipBlockComment :: Parser ()
skipBlockComment =
        string "/*"
    *>  manyTill anyChar (string "*/")
    *>  return ()

skipComment :: Parser ()
skipComment =
        skipLineComment
    <|> skipAPIComment
    <|> skipBlockComment

skipComments :: Parser ()
skipComments =
    skipMany (skipSpace >> skipComment >> skipSpace)


parseToken :: Parser Token
parseToken =
        skipSpace
    *>  skipComments
    *>
    (       (KW <$> parseKeyword)
        <|> (SY <$> parseSymbol)
        <|> (IC <$> parseIntegerConstant)
        <|> (SC <$> parseStringConstant)
        <|> (ID <$> parseIdentifier)
    )
    <*  skipSpace
    <*  skipComments
    

parseTokens :: Parser [Token]
parseTokens = manyTill parseToken endOfInput

runParseTokens :: BS.ByteString -> Either String [Token]
runParseTokens = parseOnly parseTokens
