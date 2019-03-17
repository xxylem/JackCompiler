module Syntax.Parser where

import Data.TokenModel
import Data.AnalyserModel

parseEIntegerConst :: Token -> Maybe EIntegerConstant
parseEIntegerConst t =
    case t of
        (IC i) -> Just (EIntegerConstant i)
        _      -> Nothing

parseEStringConst :: Token -> Maybe EStringConstant
parseEStringConst t =
    case t of
        (SC s) -> Just (EStringConstant s)
        _      -> Nothing

parseEKeywordConst :: Token -> Maybe EKeywordConstant
parseEKeywordConst t =
    case t of
        (KW TrueKW)     -> Just EKConTrue
        (KW FalseKW)    -> Just EKConFalse
        (KW Null)       -> Just EKConNull
        (KW This)       -> Just EKConThis
        _               -> Nothing

parseEVarName :: Token -> Maybe EVarName
parseEVarName t =
    case t of
        (ID name) -> Just (EVarName name)
        _         -> Nothing

parseOp :: Token -> Maybe Op
parseOp t =
    case t of
        (SY Plus)           -> Just OpPlus
        (SY Minus)          -> Just OpMinus
        (SY Asterix)        -> Just OpMultiply
        (SY Slash)          -> Just OpDivide
        (SY Ampersand)      -> Just OpAnd
        (SY VerticalBar)    -> Just OpOr
        (SY LAngleBracket)  -> Just OpLT
        (SY RAngleBracket)  -> Just OpGT
        (SY Equal)          -> Just OpEqual
        _                   -> Nothing

parseUnaryOp :: Token -> Maybe UnaryOp
parseUnaryOp t =
    case t of
        (SY Minus) -> Just UOPArithNegation
        (SY Tilde) -> Just UOPBitNegation
        _          -> Nothing

