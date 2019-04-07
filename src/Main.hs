{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

-- import Data.AnalyserModel
-- import Data.TokenModel
-- import Syntax.Tokeniser
-- import Syntax.Parser
-- import Compilation.TokensXMLWriter
-- import Compilation.JackClassXMLWriter

import qualified Data.Source.Model as SRC
import qualified Data.Output.Model as OUT
import qualified Data.Jack.Token.ConversionTo.ByteString.XML as TK2XML
import qualified Parser.Token as PT

import qualified Data.ByteString.Char8 as BS
import ReadArgs (readArgs)
import System.Directory (listDirectory)
import System.FilePath (hasExtension, isExtensionOf, (</>))

readJackFiles :: [FilePath] -> IO [SRC.UnparsedBSFile]
readJackFiles [] = return []
readJackFiles (f:fs) = do
    file <- BS.readFile f
    files <- readJackFiles fs
    return (SRC.toUnparsedBSFile f file : files)
    
main :: IO ()
main = do
    (path :: FilePath) <- readArgs
    if not $ hasExtension path
        then do
            dirContents <- listDirectory path
            let jackFileNames = map (path </>) $ filter (".jack" `isExtensionOf`) dirContents
            unparsedBSFiles <- readJackFiles jackFileNames
            let result = PT.tokeniseFiles unparsedBSFiles
            case result of
                Right tokenisedFiles ->
                    (OUT.writeOutputFiles . TK2XML.convertDirectory) tokenisedFiles
                Left err -> print err
    --         writeTokenisedJackFilesXML tokenisedJackFiles
    --         case runParseJackClasses tokenisedJackFiles of
    --             Right parsedJackFiles -> writeJackClassesXML parsedJackFiles
    --             Left err              -> print err
            
            
        else putStrLn "Usage: JackCompiler.exe directory\n"
    return ()

