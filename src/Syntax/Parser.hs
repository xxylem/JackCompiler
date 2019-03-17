module Syntax.Parser where

import Data.TokenModel
import Data.AnalyserModel

import Control.Applicative ((<|>))
import Data.Char (isUpper, isLower)
import qualified Data.ByteString.Char8 as BS

type TokenParser a =
        TokenisedJackFile
    ->  Maybe (a, TokenisedJackFile)

   
parseJackType :: TokenParser JackType
parseJackType ts =
        (   skipIntKw ts
        >>= \(_, ts)
        ->  return (IntType, ts))
    <|> (   skipCharKw ts
        >>= \(_, ts)
        ->  return (CharType, ts))
    <|> (   skipBoolKw ts
        >>= \(_, ts)
        ->  return (BoolType, ts))
    <|> (   parseClassName ts
        >>= \(className, ts)
        ->  return (ClassType className, ts))

parseSubroutineDec :: TokenParser SubroutineDec
parseSubroutineDec ts =
        parseSubroutineKind ts
    >>= \(srKind, ts)
    ->  parseSubroutineType ts
    >>= \(srType, ts)
    ->  parseSubroutineName ts
    >>= \(srName, ts)
    ->  parseParameters ts
    >>= \(params, ts)
    ->  parseSubroutineBody ts
    >>= \(body, ts)
    ->  return (SubroutineDec srKind
                              srType
                              srName
                              params
                              body,  ts)

parseSubroutineKind :: TokenParser SubroutineKind
parseSubroutineKind ts =
        (   skipConstructor ts
        >>= \(_, ts)
        ->  return (SRConstructor, ts))
    <|> (   skipFunction ts
        >>= \(_, ts)
        ->  return (SRFunction, ts))
    <|> (   skipMethod ts
        >>= \(_, ts)
        ->  return (SRMethod, ts))

parseSubroutineType :: TokenParser SubroutineType
parseSubroutineType ts =
        (   skipVoid ts
        >>= \(_, ts)
        ->  return (VoidType, ts))
    <|> (case parseJackType ts of
            Just (jType, ts) -> Just (SRType jType, ts)
            Nothing          -> Nothing)

parseParameters :: TokenParser [Parameter]
parseParameters ts =
        parseFirstParameter ts
    >>= \(prm, ts)
    ->  parseTailParameters ts
    >>= \(prms, ts)
    ->  return (prm:prms, ts)

parseTailParameters :: TokenParser [Parameter]
parseTailParameters ts =
    case parseTailParameter ts of
        Just (prm, ts) -> case parseTailParameters ts of
                            Just (prms, ts') -> Just (prm:prms, ts')
                            Nothing          -> Just ([prm], ts)
        Nothing        -> Just ([], ts)

parseTailParameter :: TokenParser Parameter
parseTailParameter ts =
        skipComma ts
    >>= \(_, ts)
    ->  parseFirstParameter ts

parseFirstParameter :: TokenParser Parameter
parseFirstParameter ts =
        parseJackType ts
    >>= \(jackType, ts)
    ->  parseVarName ts
    >>= \(varName, ts)
    ->  return (Param jackType varName, ts)

parseSubroutineBody :: TokenParser SubroutineBody
parseSubroutineBody ts =
        skipLCurlyBracket ts
    >>= \(_, ts)
    ->  parseVarDecs ts
    >>= \(varDecs, ts)
    ->  parseStatements ts
    >>= \(stmts, ts)
    ->  return (SubroutineBody varDecs stmts, ts)

parseVarNameListOption :: TokenParser VarName
parseVarNameListOption ts =
        skipComma ts
    >>= \(_, ts)
    ->  parseVarName ts

parseVarNameListOptions :: TokenParser [VarName]
parseVarNameListOptions ts =
        case parseVarNameListOption ts of
            Just (name, ts) -> case parseVarNameListOptions ts of
                                    Just (names, ts') -> Just (name:names, ts')
                                    Nothing           -> Just ([name], ts)
            Nothing         -> Just ([], ts)

parseVarNameList :: TokenParser [VarName]
parseVarNameList ts =
        parseVarName ts
    >>= \(name, ts)
    ->  parseVarNameListOptions ts
    >>= \(names, ts)
    ->  return (name:names, ts)

parseVarDecs :: TokenParser [VarDec]
parseVarDecs ts =
        case parseVarDec ts of
            Just (varDec, ts) -> case parseVarDecs ts of
                                    Just (varDecs, ts') -> Just (varDec:varDecs, ts')
                                    Nothing             -> Just ([varDec], ts)
            Nothing           -> Just ([], ts)

parseVarDec :: TokenParser VarDec
parseVarDec ts =
        skipVarKw ts
    >>= \(_, ts)
    ->  parseJackType ts
    >>= \(jackType, ts)
    ->  parseVarNameList ts
    >>= \(varNames, ts)
    ->  skipSemicolon ts
    >>= \(_, ts)
    ->  return (VarDec jackType varNames, ts)

parseStatements :: TokenParser [Statement]
parseStatements ts =
    case parseStatement ts of
        Just (stmt, ts) -> case parseStatements ts of
                                Just (stmts, ts') -> Just (stmt:stmts, ts')
                                Nothing           -> Just ([stmt], ts)
        Nothing -> Just ([], ts)

parseStatement :: TokenParser Statement
parseStatement ts =
        parseLetStatement ts
    <|> parseIfStatement ts
    <|> parseWhileStatement ts
    <|> parseDoStatement ts
    <|> parseReturnStatement ts

parseLetStatementName :: TokenParser LetStatementName
parseLetStatementName ts =
        (   parseEArrayExp ts
        >>= \(arr, ts) -> Just (LSA arr, ts))
    <|> (   parseVarName ts
        >>= \(varName, ts) -> Just (LSV varName, ts) )

parseLetStatement :: TokenParser Statement
parseLetStatement ts =
        skipLet ts
    >>= \(_, ts)
    ->  parseLetStatementName ts
    >>= \(name, ts)
    ->  skipEqual ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  skipSemicolon ts
    >>= \(_, ts)
    ->  return (LetStatement name expr, ts)


parseElseStatement :: TokenParser (Maybe [Statement])
parseElseStatement ts =
        (   skipElse ts
        >>= \(_, ts)
        ->  skipLCurlyBracket ts
        >>= \(_, ts)
        ->  parseStatements ts
        >>= \(stmts, ts)
        ->  skipRCurlyBracket ts
        >>= \(_, ts)
        ->  return (Just stmts, ts))
    <|> return (Nothing, ts)

parseIfStatement :: TokenParser Statement
parseIfStatement ts =
        skipIf ts
    >>= \(_, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  skipLCurlyBracket ts
    >>= \(_, ts)
    ->  parseStatements ts
    >>= \(ifStmts, ts)
    ->  skipRCurlyBracket ts
    >>= \(_, ts)
    ->  parseElseStatement ts
    >>= \(elseStmts, ts)
    ->  return (IfStatement expr ifStmts elseStmts, ts)
    

parseWhileStatement :: TokenParser Statement
parseWhileStatement ts =
        skipWhile ts
    >>= \(_, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  skipLCurlyBracket ts
    >>= \(_, ts)
    ->  parseStatements ts
    >>= \(stmts, ts)
    ->  skipRCurlyBracket ts
    >>= \(_, ts)
    ->  return (WhileStatement expr stmts, ts)

parseDoStatement :: TokenParser Statement
parseDoStatement ts =
        skipDo ts
    >>= \(_, ts)
    ->  parseSubroutineCall ts
    >>= \(subCall, ts)
    ->  skipSemicolon ts
    >>= \(_, ts)
    ->  return (DoStatement subCall, ts)

parseReturnStatement :: TokenParser Statement
parseReturnStatement ts =
        skipReturn ts
    >>= \(_, ts)
    ->  case parseExpression ts of
            Just (expr, ts') -> Just (ReturnStatement (Just expr), ts')
            Nothing          -> Just (ReturnStatement Nothing, ts)
    >>= \(retStatement, ts)
    ->  skipSemicolon ts
    >>= \(_, ts)
    ->  return (retStatement, ts)

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

parseEArrayExp :: TokenParser EArrayExp --todo create state, maybe monad comb.
parseEArrayExp ts = 
        parseVarName ts
    >>= \(varName, ts)
    ->  skipLSquareBracket ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  skipRSquareBracket ts
    >>= \(_, ts)
    ->  return (EArrayExp varName expr, ts)

parseArrayExpTerm :: TokenParser Term
parseArrayExpTerm ts =
    case parseEArrayExp ts of
        Just (res, ts) -> Just (TArrayExp res, ts)
        Nothing        -> Nothing

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
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseExpressionList ts
    >>= \(exprs, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  return (SR subName exprs, ts)



parseSubroutineCallClass :: TokenParser SubroutineCall
parseSubroutineCallClass ts =
        parseClassName ts
    >>= \(className, ts)
    ->  skipFullStop ts
    >>= \(_, ts)
    ->  parseSubroutineName ts
    >>= \(subName, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseExpressionList ts
    >>= \(exprs, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  return (SRCN className subName exprs, ts) -- todo use parseSubroutineSimple

parseSubroutineCallVar :: TokenParser SubroutineCall
parseSubroutineCallVar ts =
    parseVarName ts
    >>= \(varName, ts)
    ->  skipFullStop ts
    >>= \(_, ts)
    ->  parseSubroutineName ts
    >>= \(subName, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseExpressionList ts
    >>= \(exprs, ts)
    ->  skipRParen ts
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
        skipLParen ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  return (TParenExpression expr, ts)

parseExpressionOption :: TokenParser Expression
parseExpressionOption [] = Nothing
parseExpressionOption ts =
    case skipComma ts of
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

-- ======================= --
-- PARSERS TO SKIP SYMBOLS --
-- ======================= --

skipToken :: Token -> TokenParser ()
skipToken _ [] = Nothing
skipToken tkn (t:ts) =
    if tkn == t then Just ((), ts)
                else Nothing

skipVoid :: TokenParser ()
skipVoid =
    skipToken (KW Void)

skipFunction :: TokenParser ()
skipFunction =
    skipToken (KW Function)

skipConstructor :: TokenParser ()
skipConstructor =
    skipToken (KW Constructor)

skipMethod :: TokenParser ()
skipMethod =
    skipToken (KW Method)

skipIntKw :: TokenParser ()
skipIntKw =
    skipToken (KW Int)

skipCharKw :: TokenParser ()
skipCharKw =
    skipToken (KW Char)

skipVarKw :: TokenParser ()
skipVarKw =
    skipToken (KW Var)

skipBoolKw :: TokenParser ()
skipBoolKw =
    skipToken (KW Boolean)

skipLet :: TokenParser ()
skipLet =
    skipToken (KW Let)

skipIf :: TokenParser ()
skipIf =
    skipToken (KW If)

skipElse :: TokenParser ()
skipElse =
    skipToken (KW Else)

skipWhile :: TokenParser ()
skipWhile =
    skipToken (KW While)

skipDo :: TokenParser ()
skipDo =
    skipToken (KW Do)

skipReturn :: TokenParser ()
skipReturn =
    skipToken (KW Return)

skipComma :: TokenParser ()
skipComma =
    skipToken (SY Comma)

skipFullStop :: TokenParser ()
skipFullStop =
    skipToken (SY FullStop)

skipSemicolon :: TokenParser ()
skipSemicolon =
    skipToken (SY Semicolon)

skipEqual :: TokenParser ()
skipEqual =
    skipToken (SY Equal)

skipLParen :: TokenParser ()
skipLParen =
    skipToken (SY LParen)

skipRParen :: TokenParser ()
skipRParen =
    skipToken (SY RParen)

skipLSquareBracket :: TokenParser ()
skipLSquareBracket =
    skipToken (SY LSquareBracket)

skipRSquareBracket :: TokenParser ()
skipRSquareBracket =
    skipToken (SY RSquareBracket)

skipLCurlyBracket :: TokenParser ()
skipLCurlyBracket =
    skipToken (SY LCurlyBracket)

skipRCurlyBracket :: TokenParser ()
skipRCurlyBracket =
    skipToken (SY RCurlyBracket)