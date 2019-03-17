{-# LANGUAGE OverloadedStrings #-}

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

expressionTkns1 :: [Token]
expressionTkns1 =
    [   IC 1    ]
expressionRes1 :: Expression
expressionRes1 =
    ESingleTerm (TIntegerConstant 1)
expressionTkns2 :: [Token]
expressionTkns2 =
    [   SC "Hello"  ]
expressionRes2 :: Expression
expressionRes2 =
    ESingleTerm (TStringConstant "Hello")
expressionTkns3 :: [Token]
expressionTkns3 =
    [  KW FalseKW   ]
expressionRes3 :: Expression
expressionRes3 =
    ESingleTerm (TKeywordConstant EKConFalse)
expressionTkns4 :: [Token]
expressionTkns4 =
    [   ID "bob"    ]
expressionRes4 :: Expression
expressionRes4 =
    ESingleTerm (TVarName (VarName "bob"))
expressionTkns5 :: [Token]
expressionTkns5 =
    [   ID "bob"
    ,   SY LSquareBracket
    ] ++
    expressionTkns2 ++
    [   SY RSquareBracket    ]
expressionRes5 :: Expression
expressionRes5 =
    ESingleTerm (TArrayExp
                    (EArrayExp (VarName "bob")
                              expressionRes2))

expressionSRCTkns1 :: [Token]
expressionSRCTkns1 =
    [   ID "bob"    
    ,   SY LParen   ]
    ++ expressionTkns1
    ++ [SY Comma    ]
    ++ expressionTkns2
    ++ [SY RParen   ]
expressionSRCRes1 :: Expression
expressionSRCRes1 =
    ESingleTerm (TSubroutineCall
                    (SR (SubroutineName "bob")
                        [expressionRes1,
                         expressionRes2]))
expressionSRCTkns2 :: [Token]
expressionSRCTkns2 =
    [   ID "Bob"
    ,   SY FullStop
    ,   ID "bob"
    ,   SY LParen   ]
    ++  expressionTkns4
    ++  [SY Comma   ]
    ++  expressionTkns5
    ++  [SY Comma   ]
    ++  expressionTkns3
    ++  [SY RParen  ]
expressionSRCRes2 :: Expression
expressionSRCRes2 =
    ESingleTerm (TSubroutineCall
                    (SRCN   (ClassName "Bob")
                            (SubroutineName "bob")
                            [ expressionRes4
                            , expressionRes5
                            , expressionRes3 ]))
expressionSRCTkns3 :: [Token]
expressionSRCTkns3 =
    [   ID "bob"
    ,   SY FullStop
    ,   ID "bob"
    ,   SY LParen   ]
    ++  expressionTkns4
    ++  [SY Comma   ]
    ++  expressionTkns5
    ++  [SY Comma   ]
    ++  expressionTkns3
    ++  [SY RParen  ]
expressionSRCRes3 :: Expression
expressionSRCRes3 =
    ESingleTerm (TSubroutineCall
                    (SRVN   (VarName "bob")
                            (SubroutineName "bob")
                            [ expressionRes4
                            , expressionRes5
                            , expressionRes3 ]))

parenExpressionTkns1 :: [Token]
parenExpressionTkns1 =
    [   SY LParen
    ,   IC 10
    ,   SY Minus
    ,   IC 100
    ,   SY RParen
    ]
parenExpressionRes1 :: Term
parenExpressionRes1 =
    TParenExpression
        (ETermOpTerm (TIntegerConstant 10)
                     OpMinus
                     (TIntegerConstant 100))

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

        describe "parseExpression Test Suite" $ do

            it "parseExpression parses IntegerConstant" $ do
                runTestExpectSuccess parseExpression expressionTkns1
                    `shouldBe` expressionRes1

            it "parseExpression parses StringConstant" $ do
                runTestExpectSuccess parseExpression expressionTkns2
                    `shouldBe` expressionRes2

            it "parseExpression parses KeywordConstant" $ do
                runTestExpectSuccess parseExpression expressionTkns3
                    `shouldBe` expressionRes3

            it "parseExpression parses VarName" $ do
                runTestExpectSuccess parseExpression expressionTkns4
                    `shouldBe` expressionRes4

            it "parseExpression parses ArrayExpression" $ do
                runTestExpectSuccess parseExpression expressionTkns5
                    `shouldBe` expressionRes5

            it "parseExpression parses simple SubroutineCall" $ do
                runTestExpectSuccess parseExpression expressionSRCTkns1
                    `shouldBe` expressionSRCRes1

            it "parseExpression parses class SubroutineCall" $ do
                runTestExpectSuccess parseExpression expressionSRCTkns2
                    `shouldBe` expressionSRCRes2

            it "parseExpression parses varName SubroutineCall" $ do
                runTestExpectSuccess parseExpression expressionSRCTkns3
                    `shouldBe` expressionSRCRes3


        describe "parseParenExpression Suite" $ do

            it "parseParenExpression handles TermOpTerm" $ do
                runTestExpectSuccess parseParenExpression parenExpressionTkns1
                    `shouldBe` parenExpressionRes1
        
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
    

