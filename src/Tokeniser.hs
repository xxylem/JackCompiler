module Tokeniser where

import Model

import Data.Attoparsec.ByteString.Char8

parseKeyword :: Parser Keyword
parseKeyword = undefined

parseSymbol :: Parser Symbol
parseSymbol = undefined

parseIntegerConstant :: Parser IntegerConstant
parseIntegerConstant = undefined

parseStringConstant :: Parser StringConstant
parseStringConstant = undefined

parseIdentifier :: Parser Identifier
parseIdentifier = undefined