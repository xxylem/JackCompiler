module Test.Parser where

import Data.AnalyserModel
import Data.TokenModel
import Syntax.Parser

import Test.Hspec
import Test.QuickCheck

runTestExpectSuccess :: TokenParser a -> [Token] -> a
runTestExpectSuccess p ts =
    let Just (res, _) = p ts in
        res



expressionListSingleExpTkns1 :: [Token]
expressionListSingleExpTkns1 =
    [   IC 10
    ]
expressionListSingleExpRes1 :: [Expression]
expressionListSingleExpRes1 =
    [   ESingleTerm (TIntegerConstant 10)
    ]  

expressionListTokens1 :: [Token]
expressionListTokens1 =
    [   IC 10
    ,   SY Comma
    ,   IC 5
    ,   SY Comma
    ,   IC 4
    ]
expressionListResult1 :: [Expression]
expressionListResult1 =
    [   ESingleTerm (TIntegerConstant 10)
    ,   ESingleTerm (TIntegerConstant 5)
    ,   ESingleTerm (TIntegerConstant 4)
    ]
expressionListTokens2 :: [Token]
expressionListTokens2 =
    [   IC 10
    ,   SY Plus
    ,   IC 5
    ,   SY Comma
    ,   IC 4
    ]
expressionListResult2 :: [Expression]
expressionListResult2 =
    [   ETermOpTerm (TIntegerConstant 10) 
                    OpPlus 
                    (TIntegerConstant 5)
    ,   ESingleTerm (TIntegerConstant 4)
    ]

main :: IO ()
main = hspec $ do
    describe "Parser Test Suite" $ do
        
        describe "parseExpressionList Suite" $ do

            it "parseExpressionList fails on no expression" $ do
                parseExpressionList [SY Minus, SY Minus]
                    `shouldBe` Nothing

            it "parseExpressionList parses single item list" $ do
                runTestExpectSuccess parseExpressionList expressionListSingleExpTkns1
                    `shouldBe` expressionListSingleExpRes1

            it "parseExpressionList parses list of integers" $ do
                runTestExpectSuccess parseExpressionList expressionListTokens1
                    `shouldBe` expressionListResult1

            it "parseExpressionList parses mix of terminal and op expressions" $ do
                runTestExpectSuccess parseExpressionList expressionListTokens2
                    `shouldBe` expressionListResult2

        describe "parseOp Test Suite" $ do

            it "parseOp parses Plus" $ do
                runTestExpectSuccess parseOp [SY Plus] `shouldBe` OpPlus

            it "parseOp parses Minus" $ do
                runTestExpectSuccess parseOp [SY Minus] `shouldBe` OpMinus

            it "parseOp parses Multiply" $ do
                runTestExpectSuccess parseOp [SY Asterix] `shouldBe` OpMultiply

            it "parseOp parses Divide" $ do
                runTestExpectSuccess parseOp [SY Slash] `shouldBe` OpDivide
            
            it "parseOp parses And" $ do
                runTestExpectSuccess parseOp [SY Ampersand] `shouldBe` OpAnd

            it "parseOp parses Or" $ do
                runTestExpectSuccess parseOp [SY VerticalBar] `shouldBe` OpOr

            it "parseOp parses LT" $ do
                runTestExpectSuccess parseOp [SY LAngleBracket] `shouldBe` OpLT
            
            it "parseOp parses GT" $ do
                runTestExpectSuccess parseOp [SY RAngleBracket] `shouldBe` OpGT

            it "parseOp parses Equal" $ do
                runTestExpectSuccess parseOp [SY Equal] `shouldBe` OpEqual

        describe "UnaryOp Expressions Suite" $ do

            it "parseUnaryOp returns ArithNeg" $ do
                runTestExpectSuccess parseUnaryOp [SY Minus] `shouldBe` UOPArithNegation

            it "parseUnaryOp returns BitNeg" $ do
                runTestExpectSuccess parseUnaryOp [SY Tilde] `shouldBe` UOPBitNegation

            it "parseUnaryOpTerm negates int" $ do
                runTestExpectSuccess parseUnaryOpTerm [SY Minus, IC 10] `shouldBe` TUnaryOp UOPArithNegation (TIntegerConstant 10)

            it "parseUnaryOpTerm negates bool" $ do
                runTestExpectSuccess parseUnaryOpTerm [SY Tilde, KW TrueKW] `shouldBe` TUnaryOp UOPBitNegation (TKeywordConstant EKConTrue)

        describe "Symbol Skipping Test Suite" $ do

            it "skipComma skips comma" $ do
                runTestExpectSuccess skipComma [SY Comma] `shouldBe` ()

            it "skipFullStop skips full stop" $ do
                runTestExpectSuccess skipFullStop [SY FullStop] `shouldBe` ()

            it "skipLParen skips left paren" $ do
                runTestExpectSuccess skipLParen [SY LParen] `shouldBe` ()

            it "skipRParen skips right paren" $ do
                runTestExpectSuccess skipRParen [SY RParen] `shouldBe` ()
    
            it "skipLSquareBracket skips left sq bracket" $ do
                runTestExpectSuccess skipLSquareBracket [SY LSquareBracket] `shouldBe` ()
    
            it "skipRSquareBracket skips right sq bracket" $ do
                runTestExpectSuccess skipRSquareBracket [SY RSquareBracket] `shouldBe` ()
    

