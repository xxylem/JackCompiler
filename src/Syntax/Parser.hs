module Syntax.Parser where

import Data.TokenModel
import Data.AnalyserModel

type TokenParser a =
        TokenisedJackFile
    ->  Maybe (a, TokenisedJackFile)

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

