{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.AnalyserModel
import Data.TokenModel
import Syntax.Tokeniser

import qualified Data.ByteString.Char8 as BS
import ReadArgs (readArgs)
import System.Directory (listDirectory)
import System.FilePath (hasExtension, isExtensionOf, (</>))

getTokenisedJackFiles :: [BS.ByteString] -> [[Token]]
getTokenisedJackFiles [] = []
getTokenisedJackFiles (f:fs) =
    case runParseTokens f of
        Right parsedFile -> parsedFile : getTokenisedJackFiles fs
        Left  _          -> getTokenisedJackFiles fs

readJackFiles :: [FilePath] -> IO [BS.ByteString]
readJackFiles [] = return []
readJackFiles (f:fs) =
    (:) <$> BS.readFile f <*> readJackFiles fs
    
main :: IO [[Token]]
main = do
    (path :: FilePath) <- readArgs
    if not $ hasExtension path
        then do
            dirContents <- listDirectory path
            let jackFileNames = map (path </>) $ filter (".jack" `isExtensionOf`) dirContents
            jackFiles <- readJackFiles jackFileNames
            let tokenisedJackFiles = getTokenisedJackFiles jackFiles
            return tokenisedJackFiles
        else putStrLn "Usage: JackCompiler.exe directory\n"
             >> return [[]]

