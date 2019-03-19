{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.AnalyserModel
import Data.TokenModel
import Syntax.Tokeniser
import Syntax.Parser
import Compilation.TokensXMLWriter

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
            -- case runParseJackClasses tokenisedJackFiles of
            --     Right parsedJackFiles -> print parsedJackFiles >> return parsedJackFiles
            --     Left err              -> print err >> return []
            writeTokenisedJackFilesXML tokenisedJackFiles
            
        else putStrLn "Usage: JackCompiler.exe directory\n"

