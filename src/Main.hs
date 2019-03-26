{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.AnalyserModel
import Data.TokenModel
import Syntax.Tokeniser
import Syntax.Parser
import Compilation.TokensXMLWriter
import Compilation.JackClassXMLWriter

import Data.Hack.ASM.ConversionTo.ByteString

import qualified Data.ByteString.Char8 as BS
import ReadArgs (readArgs)
import System.Directory (listDirectory)
import System.FilePath (hasExtension, isExtensionOf, (</>))

readJackFiles :: [FilePath] -> IO [JackFileWithPath]
readJackFiles [] = return []
readJackFiles (f:fs) = do
    file <- BS.readFile f
    files <- readJackFiles fs
    return ((file, f):files)
    
main :: IO ()
main = do
    (path :: FilePath) <- readArgs
    if not $ hasExtension path
        then do
            dirContents <- listDirectory path
            let jackFileNames = map (path </>) $ filter (".jack" `isExtensionOf`) dirContents
            jackFiles <- readJackFiles jackFileNames
            let tokenisedJackFiles = tokeniseJackFiles jackFiles
            writeTokenisedJackFilesXML tokenisedJackFiles
            case runParseJackClasses tokenisedJackFiles of
                Right parsedJackFiles -> writeJackClassesXML parsedJackFiles
                Left err              -> print err
            
            
        else putStrLn "Usage: JackCompiler.exe directory\n"

