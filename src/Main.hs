{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.AnalyserModel
import Data.TokenModel
import Syntax.Tokeniser

import qualified Data.ByteString.Char8 as BS
import ReadArgs (readArgs)
import System.Directory (listDirectory)
import System.FilePath (hasExtension, isExtensionOf, (</>))

readJackFiles :: [FilePath] -> IO [JackFile]
readJackFiles [] = return []
readJackFiles (f:fs) =
    (:) <$> BS.readFile f <*> readJackFiles fs
    
main :: IO [TokenisedJackFile]
main = do
    (path :: FilePath) <- readArgs
    if not $ hasExtension path
        then do
            dirContents <- listDirectory path
            let jackFileNames = map (path </>) $ filter (".jack" `isExtensionOf`) dirContents
            jackFiles <- readJackFiles jackFileNames
            let tokenisedJackFiles = tokeniseJackFiles jackFiles
            return tokenisedJackFiles
        else putStrLn "Usage: JackCompiler.exe directory\n"
             >> return [[]]

