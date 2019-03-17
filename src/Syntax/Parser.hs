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

parseVarName :: TokenParser VarName
parseVarName [] = Nothing
parseVarName (t:ts) =
    case t of
        (ID name) -> Just (VarName name, ts)
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
        parseVarName ts
    >>= \(varName, ts)
    ->  parseLSquareBracket ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  parseRSquareBracket ts
    >>= \(_, ts)
    ->  return (EArrayExp varName expr, ts)

parseLParen :: TokenParser ()
parseLParen [] = Nothing
parseLParen (t:ts) =
    case t of
        (SY LParen) -> Just ((), ts)
        _           -> Nothing

parseRParen :: TokenParser ()
parseRParen [] = Nothing
parseRParen (t:ts) =
    case t of
        (SY RParen) -> Just ((), ts)
        _           -> Nothing

parseSubroutineName :: TokenParser SubroutineName
parseSubroutineName [] = Nothing
parseSubroutineName (t:ts) =
    case t of
        (ID i) -> Just (SubroutineName i, ts)
        _      -> Nothing

parseClassName :: TokenParser ClassName
parseClassName [] = Nothing
parseClassName (t:ts) =
    case t of
        (ID i) -> Just (ClassName i, ts)
        _      -> Nothing

parseSubroutineCallSimple :: TokenParser SubroutineCall
parseSubroutineCallSimple ts =
        parseSubroutineName ts
    >>= \(subName, ts)
    ->  parseLParen ts
    >>= \(_, ts)
    ->  parseExpressionList ts
    >>= \(exprs, ts)
    ->  parseRParen ts
    >>= \(_, ts)
    ->  return (SR subName exprs, ts)

parseFullStop :: TokenParser ()
parseFullStop [] = Nothing
parseFullStop (t:ts) =
    case t of
        (SY FullStop) -> Just ((), ts)
        _             -> Nothing

parseSubroutineCallClass :: TokenParser SubroutineCall
parseSubroutineCallClass ts =
        parseClassName ts
    >>= \(className, ts)
    ->  parseFullStop ts
    >>= \(_, ts)
    ->  parseSubroutineName ts
    >>= \(subName, ts)
    ->  parseLParen ts
    >>= \(_, ts)
    ->  parseExpressionList ts
    >>= \(exprs, ts)
    ->  parseRParen ts
    >>= \(_, ts)
    ->  return (SRCN className subName exprs, ts) -- todo use parseSubroutineSimple

parseSubroutineCallVar :: TokenParser SubroutineCall
parseSubroutineCallVar ts =
    parseVarName ts
    >>= \(varName, ts)
    ->  parseFullStop ts
    >>= \(_, ts)
    ->  parseSubroutineName ts
    >>= \(subName, ts)
    ->  parseLParen ts
    >>= \(_, ts)
    ->  parseExpressionList ts
    >>= \(exprs, ts)
    ->  parseRParen ts
    >>= \(_, ts)
    ->  return (SRVN varName subName exprs, ts) -- todo use parseSubroutineSimple

parseSubroutineCall :: TokenParser SubroutineCall
parseSubroutineCall = undefined

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

