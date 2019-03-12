{-# LANGUAGE OverloadedStrings #-}

module Tokeniser where

import Model

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8

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
        (string "class" >> return Class)
    <|> (string "constructor" >> return Constructor)
    <|> (string "function"   >> return Function)
    <|> (string "method" >> return Method)
    <|> (string "field" >> return Field)
    <|> (string "static" >> return Static)
    <|> (string "var" >> return Var)
    <|> (string "int" >> return Int)
    <|> (string "char" >> return Char)
    <|> (string "boolean" >> return Boolean)
    <|> (string "void" >> return Void)
    <|> (string "true" >> return TrueKW)
    <|> (string "false" >> return FalseKW)
    <|> (string "null" >> return Null)
    <|> (string "this" >> return This)
    <|> (string "let" >> return Let)
    <|> (string "do" >> return Do)
    <|> (string "if" >> return If)
    <|> (string "else" >> return Else)
    <|> (string "while" >> return While)
    <|> (string "return" >> return Return)
    )
    <* take1Space


parseSymbol :: Parser Symbol
parseSymbol =
    (
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
    )

parseIntegerConstant :: Parser IntegerConstant
parseIntegerConstant = undefined

parseStringConstant :: Parser StringConstant
parseStringConstant = undefined

parseIdentifier :: Parser Identifier
parseIdentifier = undefined

parseToken :: Parser Token
parseToken = undefined