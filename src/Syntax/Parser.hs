module Syntax.Parser where

import Data.TokenModel
import Data.AnalyserModel

{-
Terminals Whenever a terminal language element of type xxx is encountered, the
syntax analyzer should generate the marked-up output:
hxxxi terminal h/xxxi
Where xxx is one of the five token types recognized by the Jack language (as
specified in the Jack grammar’s ‘‘lexical elements’’ section), namely, keyword,
symbol, integerConstant, stringConstant, or identifier.
-}

parseTokenToOp :: Token -> Maybe Op
parseTokenToOp t =
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

parseTokenToUnaryOp :: Token -> Maybe UnaryOp
parseTokenToUnaryOp t =
    case t of
        (SY Minus) -> Just UOPArithNegation
        (SY Tilde) -> Just UOPBitNegation
        _          -> Nothing

parseTokenToEIntegerConst :: Token -> Maybe EIntegerConstant
parseTokenToEIntegerConst t =
    case t of
        (IC i) -> Just (EIntegerConstant i)
        _      -> Nothing

parseTokenToEStringConst :: Token -> Maybe EStringConstant
parseTokenToEStringConst t =
    case t of
        (SC s) -> Just (EStringConstant s)
        _      -> Nothing

parseTokenToExpKeywordConst :: Token -> Maybe ExpKeywordConstant
parseTokenToExpKeywordConst t =
    case t of
        (KW TrueKW)     -> Just EKConTrue
        (KW FalseKW)    -> Just EKConFalse
        (KW Null)       -> Just EKConNull
        (KW This)       -> Just EKConThis
        _               -> Nothing