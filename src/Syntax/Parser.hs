{-# LANGUAGE FlexibleInstances #-}

module Syntax.Parser where

import Data.TokenModel
import Data.AnalyserModel

import Control.Applicative
import Data.Char (isUpper, isLower)
import qualified Data.ByteString.Char8 as BS

type TokenParser a =
        TokenisedJackFile
    ->  Either (String, TokenisedJackFile) (a, TokenisedJackFile)

runParseJackClasses :: [TokenisedJackFileWithPath] -> Either (String, TokenisedJackFile) [JackClassWithPath]
runParseJackClasses [] = Right []
runParseJackClasses ((file, path):fs) = do
    jackClass <- runParseJackClass file
    jackClasses <- runParseJackClasses fs
    return ((jackClass, path):jackClasses)

runParseJackClass :: TokenisedJackFile -> Either (String, TokenisedJackFile) JackClass
runParseJackClass f =
        parseJackClass f
    >>= \(jackClass, _)
    ->  return jackClass

parseJackClass :: TokenParser JackClass
parseJackClass ts =
        skipClass ts
    >>= \(_, ts)
    ->  parseClassName ts
    >>= \(cName, ts)
    ->  skipLCurlyBracket ts
    >>= \(_, ts)
    ->  parseClassVarDecs ts
    >>= \(cVarDecs, ts)
    ->  parseSubroutineDecs ts
    >>= \(srDecs, ts)
    ->  skipRCurlyBracket ts
    >>= \(_, ts)
    ->  checkEndOfInput ts
    >>= \(_, ts)
    ->  return (JackClass cName
                          cVarDecs
                          srDecs, ts)

checkEndOfInput :: TokenParser ()
checkEndOfInput [] = Right ((), [])
checkEndOfInput ts = Left ("should be end of file", ts)

parseClassVarDecs :: TokenParser [ClassVarDec]
parseClassVarDecs ts =
    case parseClassVarDec ts of
        Right (cVarDec, ts) -> case parseClassVarDecs ts of
                                Right (cVarDecs, ts') -> Right (cVarDec:cVarDecs, ts')
                                Left _              -> Right ([cVarDec], ts)
        Left _            -> Right ([], ts)

parseClassVarDec :: TokenParser ClassVarDec
parseClassVarDec ts =
        parseClassVarKind ts
    >>= \(cVarKind, ts)
    ->  parseJackType ts
    >>= \(jType, ts)
    ->  parseVarNameList ts
    >>= \(vNames, ts)
    ->  skipSemicolon ts
    >>= \(_, ts)
    ->  return (ClassVarDec cVarKind
                            jType
                            vNames,
                            ts)

parseClassVarKind :: TokenParser ClassVarKind
parseClassVarKind ts =
        (   skipStatic ts
        >>= \(_, ts)
        ->  return (CVStatic, ts))
    <|> (   skipField ts
        >>= \(_, ts)
        ->  return (CVField, ts))

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

parseSubroutineDecs :: TokenParser [SubroutineDec]
parseSubroutineDecs ts =
    case parseSubroutineDec ts of
        Right (srDec, ts) -> case parseSubroutineDecs ts of
                                Right (srDecs, ts') -> Right (srDec:srDecs, ts')
                                Left _            -> Right ([srDec], ts)
        Left _          -> Right ([], ts)

parseSubroutineDec :: TokenParser SubroutineDec
parseSubroutineDec ts =
        parseSubroutineKind ts
    >>= \(srKind, ts)
    ->  parseSubroutineType ts
    >>= \(srType, ts)
    ->  parseSubroutineName ts
    >>= \(srName, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseParameters ts
    >>= \(params, ts)
    ->  skipRParen ts
    >>= \(_, ts)
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
            Right (jType, ts) -> Right (SRType jType, ts)
            Left (err, errTkns)          -> Left (err <> " parseSubroutineType ", errTkns))

parseParameters :: TokenParser [Parameter]
parseParameters ts =
        case parseFirstParameter ts of
            Right (prm, ts) ->  parseTailParameters ts
                                        >>= \(prms, ts)
                                        ->  return (prm:prms, ts)
            Left _ -> return ([], ts)


parseTailParameters :: TokenParser [Parameter]
parseTailParameters ts =
    case parseTailParameter ts of
        Right (prm, ts) -> case parseTailParameters ts of
                            Right (prms, ts') -> Right (prm:prms, ts')
                            Left _          -> Right ([prm], ts)
        Left _        -> Right ([], ts)

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
    ->  skipRCurlyBracket ts
    >>= \(_, ts)
    ->  return (SubroutineBody varDecs stmts, ts)

parseVarNameListOption :: TokenParser VarName
parseVarNameListOption ts =
        skipComma ts
    >>= \(_, ts)
    ->  parseVarName ts

parseVarNameListOptions :: TokenParser [VarName]
parseVarNameListOptions ts =
        case parseVarNameListOption ts of
            Right (name, ts) -> case parseVarNameListOptions ts of
                                    Right (names, ts') -> Right (name:names, ts')
                                    Left _           -> Right ([name], ts)
            Left _         -> Right ([], ts)

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
            Right (varDec, ts) -> case parseVarDecs ts of
                                    Right (varDecs, ts') -> Right (varDec:varDecs, ts')
                                    Left _             -> Right ([varDec], ts)
            Left _           -> Right ([], ts)

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
        Right (stmt, ts) -> case parseStatements ts of
                                Right (stmts, ts') -> Right (stmt:stmts, ts')
                                Left _           -> Right ([stmt], ts)
        Left _ -> Right ([], ts)

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
        >>= \(arr, ts) -> Right (LSA arr, ts))
    <|> (   parseVarName ts
        >>= \(varName, ts) -> Right (LSV varName, ts) )

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
            Right (expr, ts') -> Right (ReturnStatement (Just expr), ts')
            Left _          -> Right (ReturnStatement Nothing, ts)
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
        Right (res, ts) -> Right (ESingleTerm res, ts)
        Left (err, errTkns)        -> Left (err <> " parseSingleTermExp ", errTkns)

instance Alternative (Either (String, TokenisedJackFile)) where
    Right res <|> _ = Right res
    Left _    <|> Right res = Right res
    Left (err, errTkns)  <|> Left (err', _) = Left (err <> " + " <> err', errTkns)

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
parseIntegerConstant [] = Left ("expected input in parseIntegerConstant", [])
parseIntegerConstant (t:ts) =
    case t of
        (IC i) -> Right (TIntegerConstant i, ts)
        _      -> Left ("failed parse in parseIntegerConstant", t:ts)

parseStringConstant :: TokenParser Term
parseStringConstant [] = Left ("expected input in parseStringConstant", [])
parseStringConstant (t:ts) =
    case t of
        (SC s) -> Right (TStringConstant s, ts)
        _      -> Left ("failed parse in parseIntegerConstant", t:ts)

parseEKeywordConstant :: TokenParser EKeywordConstant
parseEKeywordConstant [] = Left ("expected input in parseEKeywordConstant", [])
parseEKeywordConstant (t:ts) =
    case t of
        (KW TrueKW)     -> Right (EKConTrue, ts)
        (KW FalseKW)    -> Right (EKConFalse, ts)
        (KW Null)       -> Right (EKConNull, ts)
        (KW This)       -> Right (EKConThis, ts)
        _               -> Left ("failed parse in parseEKeywordConstant", t:ts)

parseKeywordConstantTerm :: TokenParser Term
parseKeywordConstantTerm ts =
    case parseEKeywordConstant ts of
        Right (res, ts) -> Right (TKeywordConstant res, ts)
        Left (err, errTkns)        -> Left (err <> " parseKeywordConstantTerm ", errTkns)

parseVarName :: TokenParser VarName
parseVarName [] = Left ("expected input in parseVarName", [])
parseVarName (t:ts) =
    case t of
        (ID name) -> Right (VarName name, ts)
        _         -> Left ("failed parse in parseVarName", t:ts)

parseVarNameTerm :: TokenParser Term
parseVarNameTerm ts = case parseVarName ts of
    Right (varName, ts) -> Right (TVarName varName, ts)
    Left (err, errTkns)            -> Left (err <> " parseVarNameTerm ", errTkns)

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
        Right (res, ts) -> Right (TArrayExp res, ts)
        Left (err, errTkns)        -> Left (err <> " parseArrayExpTerm ", errTkns)

parseSubroutineName :: TokenParser SubroutineName
parseSubroutineName [] = Left ("expected input in parseSubroutineName", [])
parseSubroutineName (t:ts) =
    case t of
        (ID i) -> Right (SubroutineName i, ts)
        _            -> Left ("failed parse in parseSubroutineName", t:ts)

parseClassName :: TokenParser ClassName
parseClassName [] = Left ("expected input in parseClassName", [])
parseClassName (t:ts) =
    case t of
        (ID i) -> Right (ClassName i, ts)
        _            -> Left ("failed parse in parseClassName", t:ts)

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
            Right res -> Right res
            Left _ -> case parseSubroutineCallVar ts of
                Right res -> Right res
                Left _ -> case parseSubroutineCallSimple ts of
                    Right res -> Right res
                    Left _ -> Left ("failed parse in parseSubroutineCall", ts)

parseSubroutineCallTerm :: TokenParser Term
parseSubroutineCallTerm ts =
    case parseSubroutineCall ts of
        Right (res, ts) -> Right (TSubroutineCall res, ts)
        Left (err, errTkns)        -> Left (err <> " parseSubroutineCallTerm ", errTkns)


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
parseExpressionOption [] = Left ("expected input in parseExpressionOption", [])
parseExpressionOption ts =
    case skipComma ts of
        Right (_, ts) -> parseExpression ts
        Left (err, errTkns)      -> Left (err <> " parseExpressionOption ", errTkns)

parseExpressionsOption :: TokenParser [Expression]
parseExpressionsOption ts =
    case parseExpressionOption ts of
        Right (expr, ts) -> case parseExpressionsOption ts of
                            Right (rsf, ts') -> Right (expr:rsf, ts')
        Left _         -> Right ([], ts)

parseExpressionList :: TokenParser [Expression]
parseExpressionList [] = Left ("expected input in parseExpressionList", [])
parseExpressionList ts =
        case parseExpression ts of Right (expr, ts) -> parseExpressionsOption ts
                                                            >>= \(exprs, ts)
                                                            ->  return (expr:exprs, ts)
                                   Left _ -> Right ([], ts)
    
    

parseOp :: TokenParser Op
parseOp [] = Left ("expected input in parseOp", [])
parseOp (t:ts) =
    case t of
        (SY Plus)           -> Right (OpPlus, ts)
        (SY Minus)          -> Right (OpMinus, ts)
        (SY Asterix)        -> Right (OpMultiply, ts)
        (SY Slash)          -> Right (OpDivide, ts)
        (SY Ampersand)      -> Right (OpAnd, ts)
        (SY VerticalBar)    -> Right (OpOr, ts)
        (SY LAngleBracket)  -> Right (OpLT, ts)
        (SY RAngleBracket)  -> Right (OpGT, ts)
        (SY Equal)          -> Right (OpEqual, ts)
        _                   -> Left ("failed parse in parseOp", ts)

parseUnaryOp :: TokenParser UnaryOp
parseUnaryOp [] = Left ("expected input in parseUnaryOp", [])
parseUnaryOp (t:ts) =
    case t of
        (SY Minus) -> Right (UOPArithNegation, ts)
        (SY Tilde) -> Right (UOPBitNegation, ts)
        _          -> Left ("failed parse in parseUnaryOp", ts)

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
skipToken _ [] = Left ("expected input in skipToken", [])
skipToken tkn (t:ts) =
    if tkn == t then Right ((), ts)
                else Left ("failed parse in skipToken. "
                            <> "expected: "
                            <> show tkn
                            <> "\n"
                            <> "got: "
                            <> show t
                            <> "\n", t:ts)

skipVoid :: TokenParser ()
skipVoid =
    skipToken (KW Void)

skipField :: TokenParser ()
skipField =
    skipToken (KW Field)

skipClass :: TokenParser ()
skipClass =
    skipToken (KW Class)

skipStatic :: TokenParser ()
skipStatic =
    skipToken (KW Static)

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