module Syntax.Parser where

import Data.TokenModel
import Data.AnalyserModel

import Control.Applicative ((<|>))
import Data.Char (isUpper, isLower)
import qualified Data.ByteString.Char8 as BS


type TokenParser a =
        TokenisedJackFile
    ->  Maybe (a, TokenisedJackFile)

parseExpression :: TokenParser Expression
parseExpression ts =
        parseTermOpTermExp ts
    <|> parseSingleTermExp ts

parseTermOpTermExp :: TokenParser Expression
parseTermOpTermExp ts =
        parseTerm ts
    >>= \(term1, ts)
    ->  parseOp ts
    >>= \(op, ts)
    ->  parseTerm ts
    >>= \(term2, ts)
    ->  return (ETermOpTerm term1 op term2, ts)

parseSingleTermExp :: TokenParser Expression
parseSingleTermExp ts =
    case parseTerm ts of
        Just (res, ts) -> Just (ESingleTerm res, ts)
        Nothing        -> Nothing

parseTerm :: TokenParser Term
parseTerm ts =
        parseIntegerConstant ts
    <|> parseStringConstant ts
    <|> parseKeywordConstantTerm ts
    <|> parseArrayExpTerm ts
    <|> parseSubroutineCallTerm ts
    <|> parseParenExpression ts
    <|> parseUnaryOpTerm ts
    <|> parseVarNameTerm ts

parseIntegerConstant :: TokenParser Term
parseIntegerConstant [] = Nothing
parseIntegerConstant (t:ts) =
    case t of
        (IC i) -> Just (TIntegerConstant i, ts)
        _      -> Nothing

parseStringConstant :: TokenParser Term
parseStringConstant [] = Nothing
parseStringConstant (t:ts) =
    case t of
        (SC s) -> Just (TStringConstant s, ts)
        _      -> Nothing

parseEKeywordConstant :: TokenParser EKeywordConstant
parseEKeywordConstant [] = Nothing
parseEKeywordConstant (t:ts) =
    case t of
        (KW TrueKW)     -> Just (EKConTrue, ts)
        (KW FalseKW)    -> Just (EKConFalse, ts)
        (KW Null)       -> Just (EKConNull, ts)
        (KW This)       -> Just (EKConThis, ts)
        _               -> Nothing

parseKeywordConstantTerm :: TokenParser Term
parseKeywordConstantTerm ts =
    case parseEKeywordConstant ts of
        Just (res, ts) -> Just (TKeywordConstant res, ts)
        Nothing        -> Nothing

parseVarName :: TokenParser VarName
parseVarName [] = Nothing
parseVarName (t:ts) =
    case t of
        (ID name) -> if isLower $ BS.head name then Just (VarName name, ts) else Nothing
        _               -> Nothing

parseVarNameTerm :: TokenParser Term
parseVarNameTerm ts = case parseVarName ts of
    Just (varName, ts) -> Just (TVarName varName, ts)
    Nothing            -> Nothing

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

parseArrayExpTerm :: TokenParser Term
parseArrayExpTerm ts =
    case parseEArrayExp ts of
        Just (res, ts) -> Just (TArrayExp res, ts)
        Nothing        -> Nothing

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
        (ID i) -> if isLower $ BS.head i then Just (SubroutineName i, ts) else Nothing
        _            -> Nothing

parseClassName :: TokenParser ClassName
parseClassName [] = Nothing
parseClassName (t:ts) =
    case t of
        (ID i) -> if isUpper $ BS.head i then Just (ClassName i, ts) else Nothing
        _            -> Nothing

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
parseSubroutineCall ts =
        case parseSubroutineCallClass ts of
            Just res -> Just res
            Nothing -> case parseSubroutineCallVar ts of
                Just res -> Just res
                Nothing -> case parseSubroutineCallSimple ts of
                    Just res -> Just res
                    Nothing -> Nothing

parseSubroutineCallTerm :: TokenParser Term
parseSubroutineCallTerm ts =
    case parseSubroutineCall ts of
        Just (res, ts) -> Just (TSubroutineCall res, ts)
        Nothing        -> Nothing


parseParenExpression :: TokenParser Term
parseParenExpression ts =
        parseLParen ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  parseRParen ts
    >>= \(_, ts)
    ->  return (TParenExpression expr, ts)

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

parseUnaryOpTerm :: TokenParser Term
parseUnaryOpTerm ts =
        parseUnaryOp ts
    >>= \(uop, ts)
    ->  parseTerm ts
    >>= \(term, ts)
    ->  return (TUnaryOp uop term, ts)