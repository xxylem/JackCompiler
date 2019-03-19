{-# LANGUAGE OverloadedStrings #-}

module Compilation.TokensXMLWriter where

import Data.TokenModel

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion (toByteString')
import System.IO
import System.FilePath (addExtension, dropExtension)

writeTokenisedJackFilesXML :: [TokenisedJackFileWithPath] -> IO ()
writeTokenisedJackFilesXML [] = return ()
writeTokenisedJackFilesXML (f:fs) =
    writeTokenisedJackFileXML f >> writeTokenisedJackFilesXML fs

writeTokenisedJackFileXML :: TokenisedJackFileWithPath -> IO ()
writeTokenisedJackFileXML (file, path) =
    withFile (toXMLTokenFilePath path) WriteMode (\h ->
        let writeTokens [] = return ()
            writeTokens (t:ts) =
                BS.hPutStrLn h (toXML t)
                >>  writeTokens ts in
        do  BS.hPutStrLn h "<tokens>"
            writeTokens file
            BS.hPutStrLn h "</tokens>")

toXMLTokenFilePath :: FilePath -> FilePath
toXMLTokenFilePath fp =
    addExtension ((dropExtension fp) <> "T") ".xml"

class ToXML a where
    toXML :: a -> BS.ByteString

instance ToXML Keyword where
    toXML t =
        case t of
            Class -> "class"
            Constructor -> "constructor"
            Function -> "function"
            Method -> "method"
            Field -> "field"
            Static -> "static"
            Var -> "var"
            Int -> "int"
            Char -> "char"
            Boolean -> "boolean"
            Void -> "void"
            TrueKW -> "true"
            FalseKW -> "false"
            Null -> "null"
            This -> "this"
            Let -> "let"
            Do -> "do"
            If -> "if"
            Else -> "else"
            While -> "while"
            Return -> "return"

instance ToXML Symbol where
    toXML sym =
        case sym of
            LCurlyBracket -> "{"
            RCurlyBracket -> "}"
            LParen -> "("
            RParen -> ")"
            LSquareBracket -> "["
            RSquareBracket -> "]"
            FullStop -> "."
            Comma -> ","
            Semicolon -> ";"
            Plus -> "+"
            Minus -> "-"
            Asterix -> "*"
            Slash -> "/"
            Ampersand -> "&amp;"
            VerticalBar -> "|"
            LAngleBracket -> "&lt;"
            RAngleBracket -> "&gt;"
            Equal -> "="
            Tilde -> "~"

instance ToXML Token where
    toXML (KW kw) = 
            "<keyword> "
        <>  toXML kw
        <>  " </keyword>"
    toXML (SY sy) = 
            "<symbol> "
        <>  toXML sy
        <>  " </symbol>"
    toXML (IC ic) = 
            "<integerConstant> "
        <>  toByteString' ic
        <>  " </integerConstant>"
    toXML (SC sc) =
            "<stringConstant> "
        <>  sc
        <>  " </stringConstant>"
    toXML (ID iden) =
            "<identifier> "
        <>  iden
        <>  " </identifier>"

