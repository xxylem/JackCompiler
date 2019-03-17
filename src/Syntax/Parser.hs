module Syntax.Parser where

import Data.TokenModel
import Data.AnalyserModel

type TokenParser a =
        TokenisedJackFile
    ->  Maybe (a, TokenisedJackFile)

parseExpression :: TokenParser Expression
parseExpression = undefined

parseEIntegerConst :: TokenParser EIntegerConstant
parseEIntegerConst [] = Nothing
parseEIntegerConst (t:ts) =
    case t of
        (IC i) -> Just (EIntegerConstant i, ts)
        _      -> Nothing

parseEStringConst :: TokenParser EStringConstant
parseEStringConst [] = Nothing
parseEStringConst (t:ts) =
    case t of
        (SC s) -> Just (EStringConstant s, ts)
        _      -> Nothing

parseEKeywordConst :: TokenParser EKeywordConstant
parseEKeywordConst [] = Nothing
parseEKeywordConst (t:ts) =
    case t of
        (KW TrueKW)     -> Just (EKConTrue, ts)
        (KW FalseKW)    -> Just (EKConFalse, ts)
        (KW Null)       -> Just (EKConNull, ts)
        (KW This)       -> Just (EKConThis, ts)
        _               -> Nothing

parseEVarName :: TokenParser EVarName
parseEVarName [] = Nothing
parseEVarName (t:ts) =
    case t of
        (ID name) -> Just (EVarName name, ts)
        _         -> Nothing

parseLSquareBracket :: TokenParser ()
parseLSquareBracket [] = Nothing
parseLSquareBracket (t:ts) =
    case t of
        (SY LSquareBracket) -> Just ((), ts)
        _                   -> Nothing

parseRSquareBracket :: TokenParser ()
parseRSquareBracket [] = Nothing
parseRSquareBracket (t:ts) =
    case t of
        (SY RSquareBracket) -> Just ((), ts)
        _                   -> Nothing

parseEArrayExp :: TokenParser EArrayExp --todo create state, maybe monad comb.
parseEArrayExp ts = 
        parseEVarName ts
    >>= \(varName, ts)
    ->  parseLSquareBracket ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  parseRSquareBracket ts
    >>= \(_, ts)
    ->  return (EArrayExp varName expr, ts)

parseComma :: TokenParser ()
parseComma [] = Nothing
parseComma (t:ts) =
    case t of
        (SY Comma) -> Just ((), ts)
        _          -> Nothing

parseExpressionOption :: TokenParser Expression
parseExpressionOption [] = Nothing
parseExpressionOption ts =
    case parseComma ts of
        Just (_, ts) -> parseExpression ts
        Nothing      -> Nothing

parseExpressionsOption :: TokenParser [Expression]
parseExpressionsOption ts =
    case parseExpressionOption ts of
        Just (expr, ts) -> case parseExpressionsOption ts of
                            Just (rsf, ts') -> Just (expr:rsf, ts')
        Nothing         -> Just ([], ts)

parseExpressionList :: TokenParser [Expression]
parseExpressionList [] = Nothing
parseExpressionList ts =
        parseExpression ts
    >>= \(expr, ts)
    ->  parseExpressionsOption ts
    >>= \(exprs, ts)
    ->  return (expr:exprs, ts)
    

parseOp :: TokenParser Op
parseOp [] = Nothing
parseOp (t:ts) =
    case t of
        (SY Plus)           -> Just (OpPlus, ts)
        (SY Minus)          -> Just (OpMinus, ts)
        (SY Asterix)        -> Just (OpMultiply, ts)
        (SY Slash)          -> Just (OpDivide, ts)
        (SY Ampersand)      -> Just (OpAnd, ts)
        (SY VerticalBar)    -> Just (OpOr, ts)
        (SY LAngleBracket)  -> Just (OpLT, ts)
        (SY RAngleBracket)  -> Just (OpGT, ts)
        (SY Equal)          -> Just (OpEqual, ts)
        _                   -> Nothing

parseUnaryOp :: TokenParser UnaryOp
parseUnaryOp [] = Nothing
parseUnaryOp (t:ts) =
    case t of
        (SY Minus) -> Just (UOPArithNegation, ts)
        (SY Tilde) -> Just (UOPBitNegation, ts)
        _          -> Nothing

