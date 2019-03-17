module Test.Parser where

import Data.AnalyserModel
import Data.TokenModel
import Syntax.Parser

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Parser Test Suite" $ do
        



        it "parseUnaryOp returns ArithNeg" $ do
            let Just (res, _) = parseUnaryOp [SY Minus] 
            res `shouldBe` UOPArithNegation

        