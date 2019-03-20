{-# LANGUAGE OverloadedStrings #-}

module Compilation.JackClassXMLWriter where

import Data.AnalyserModel

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion (toByteString')
import System.FilePath (addExtension, dropExtension)

writeJackClassesXML :: [JackClassWithPath] -> IO ()
writeJackClassesXML [] = return ()
writeJackClassesXML (c:cs) =
    writeJackClassXML c >> writeJackClassesXML cs

writeJackClassXML :: JackClassWithPath -> IO ()
writeJackClassXML (jc, path) =
    BS.writeFile (xmlPath path) (toXML jc)
    where xmlPath p = addExtension (dropExtension p) "xml"

identifierXML :: BS.ByteString -> BS.ByteString
identifierXML i =
    "<identifier> " <> i <> " </identifier>\n"

keywordXML :: BS.ByteString -> BS.ByteString
keywordXML kw =
    "<keyword> " <> kw <> " </keyword>\n"

symbolXML :: BS.ByteString -> BS.ByteString
symbolXML sym =
    "<symbol> " <> sym <> " </symbol>\n"

class ToXML a where
    toXML :: a -> BS.ByteString

instance ToXML JackClass where
    toXML (JackClass cName cvDecs srDecs) =
            "<class>\n"
        <>  keywordXML "class"
        <>  toXML cName
        <>  symbolXML "{"
        <>  foldr ((<>) . toXML) "" cvDecs
        <>  foldr ((<>) . toXML) "" srDecs
        <>  symbolXML "}"
        <>  " </class>\n"

instance ToXML ClassVarDec where
    toXML (ClassVarDec cvKind jType vNames) =
            "<classVarDec>\n"
        <>  toXML cvKind
        <>  toXML jType
        <>  varNameListXML vNames
        <>  symbolXML ";"
        <>  " </classVarDec>\n"

instance ToXML ClassVarKind where
    toXML CVStatic = keywordXML "static"
    toXML CVField = keywordXML "field"

instance ToXML SubroutineDec where
    toXML (SubroutineDec sKind
                         sType
                         sName
                         params
                         sBody) =
            "<subroutineDec>\n"
        <>  toXML sKind
        <>  toXML sType
        <>  toXML sName
        <>  symbolXML "("
        <>  paramListXML params
        <>  symbolXML ")"
        <>  toXML sBody
        <>  " </subroutineDec>\n"

instance ToXML SubroutineKind where
    toXML SRConstructor = keywordXML "constructor"
    toXML SRFunction = keywordXML "function"
    toXML SRMethod = keywordXML "method"
    
instance ToXML SubroutineType where
    toXML VoidType = keywordXML "void"
    toXML (SRType jType) = toXML jType

paramListXML :: [Parameter] -> BS.ByteString
paramListXML ps =
        "<parameterList>\n"
    <>  go ps
    <>  " </parameterList>\n"
        where go [] = ""
              go (p:ps) =
                toXML p <> go' ps
              go' [] = ""
              go' (p':ps') =
                    symbolXML ","
                <>  toXML p'
                <>  go' ps'
        

instance ToXML Parameter where
    toXML (Param jType vName) =
            toXML jType
        <>  toXML vName

instance ToXML SubroutineBody where
    toXML (SubroutineBody varDecs stmts) =
            "<subroutineBody>\n"
        <>  symbolXML "{"
        <>  varDecListXML varDecs
        <>  statementListXML stmts
        <>  symbolXML "}"
        <>  " </subroutineBody>\n"

varDecListXML :: [VarDec] -> BS.ByteString
varDecListXML = foldr ((<>) . toXML) ""
        
instance ToXML VarDec where
    toXML (VarDec jType vNames) =
            "<varDec>\n"
        <>  keywordXML "var"
        <>  toXML jType
        <>  varNameListXML vNames
        <>  symbolXML ";"
        <>  " </varDec>\n"

varNameListXML :: [VarName] -> BS.ByteString
varNameListXML [] = ""
varNameListXML (v:vs) =
    toXML v <> go vs
        where go [] = ""
              go (v':vs') =
                    symbolXML ","
                <>  toXML v'
                <>  go vs'

instance ToXML JackType where
    toXML IntType = keywordXML "int"
    toXML CharType = keywordXML "char"
    toXML BoolType = keywordXML "boolean"
    toXML (ClassType clName) = toXML clName

instance ToXML Statement where
    toXML (LetStatement lsName expr) =
            "<letStatement>\n"
        <>  keywordXML "let"
        <>  toXML lsName
        <>  symbolXML "="
        <>  toXML expr
        <>  symbolXML ";"
        <>  " </letStatement>\n"
    toXML (IfStatement expr stmts elseStmts) =
            "<ifStatement>\n"
        <>  keywordXML "if"
        <>  symbolXML "("
        <>  toXML expr
        <>  symbolXML ")"
        <>  symbolXML "{"
        <>  statementListXML stmts
        <>  symbolXML "}"
        <>  (case elseStmts of
            Just ss -> keywordXML "else"
                    <> symbolXML "{"
                    <> statementListXML ss
                    <> symbolXML "}"
            Nothing -> "")
        <>  " </ifStatement>\n"
    toXML (WhileStatement expr stmts) =
            "<whileStatement>\n"
        <>  keywordXML "while"
        <>  symbolXML "("
        <>  toXML expr
        <>  symbolXML ")"
        <>  symbolXML "{"
        <>  statementListXML stmts
        <>  symbolXML "}"
        <>  " </whileStatement>\n"
    toXML (DoStatement srCall) =
            "<doStatement>\n"
        <>  keywordXML "do"
        <>  toXML srCall
        <>  symbolXML ";"
        <>  " </doStatement>\n"
    toXML (ReturnStatement mbExpr) =
            "<returnStatement>\n"
        <>  keywordXML "return"
        <>  (case mbExpr of
            Just expr -> toXML expr
            Nothing   -> "")
        <>  symbolXML ";"
        <>  " </returnStatement>\n"


statementListXML :: [Statement] -> BS.ByteString
statementListXML stmts =
        "<statements>\n"
    <>  foldr ((<>) . toXML) "" stmts
    <>  " </statements>\n"

instance ToXML LetStatementName where
    toXML (LSV vName) = toXML vName
    toXML (LSA eArrExp) = toXML eArrExp

instance ToXML Expression where
    toXML e =
            "<expression>\n"
        <>  (case e of
            ESingleTerm t -> toXML t
            ETermOpTerm t1 op t2 ->
                    toXML t1
                <>  toXML op
                <>  toXML t2)
        <>  " </expression>\n"

instance ToXML Term where
    toXML term =
            "<term>\n"
        <>  (case term of
            TIntegerConstant i ->
                    "<integerConstant>"
                <>  toByteString' i
                <>  " </integerConstant>\n"
            TStringConstant s ->
                    "<stringConstant>"
                <>  s
                <>  " </stringConstant>\n"
            TKeywordConstant kw ->
                toXML kw
            TVarName varName ->
                toXML varName
            TArrayExp eArrExp ->
                toXML eArrExp
            TSubroutineCall sc ->
                toXML sc
            TParenExpression expr ->
                    symbolXML "("
                <>  toXML expr
                <>  symbolXML ")"
            TUnaryOp uop uoTerm ->
                    toXML uop
                <>  toXML uoTerm)
        <>  " </term>\n"

instance ToXML VarName where
    toXML (VarName varName) =
        identifierXML varName

instance ToXML EKeywordConstant where
    toXML EKConTrue = keywordXML "true"
    toXML EKConFalse = keywordXML "false"
    toXML EKConNull = keywordXML "null"
    toXML EKConThis = keywordXML "this"

instance ToXML EArrayExp where
    toXML (EArrayExp varName expr) =
            toXML varName
        <>  symbolXML "["
        <>  toXML expr
        <>  symbolXML "]"

instance ToXML SubroutineName where
    toXML (SubroutineName srName) =
        identifierXML srName

instance ToXML ClassName where
    toXML (ClassName clName) =
        identifierXML clName

instance ToXML SubroutineCall where
    toXML (SR srName exprs) =
            toXML srName
        <>  symbolXML "("
        <>  expressionListXML exprs
        <>  symbolXML ")"
    toXML (SRCN clName srName exprs) =
            toXML clName
        <>  symbolXML "."
        <>  toXML srName
        <>  symbolXML "("
        <>  expressionListXML exprs
        <>  symbolXML ")"
    toXML (SRVN vName srName exprs) =
            toXML vName
        <>  symbolXML "."
        <>  toXML srName
        <>  symbolXML "("
        <>  expressionListXML exprs
        <>  symbolXML ")"

expressionListXML :: [Expression] -> BS.ByteString
expressionListXML exprs =
        "<expressionList>\n"
    <>  go exprs
    <>  " </expressionList>\n"
    where go [] = ""
          go (e:es) =
            toXML e <> go' es
          go' [] = ""
          go' (e':es') =
                symbolXML "," 
            <>  toXML e'
            <>  go' es'

instance ToXML Op where
    toXML OpPlus = symbolXML "+"
    toXML OpMinus = symbolXML "-"
    toXML OpMultiply = symbolXML "*"
    toXML OpDivide = symbolXML "/"
    toXML OpAnd = symbolXML "&amp;"
    toXML OpOr = symbolXML "|"
    toXML OpLT = symbolXML "&lt;"
    toXML OpGT = symbolXML "&gt;"
    toXML OpEqual = symbolXML "="

instance ToXML UnaryOp where
    toXML UOPArithNegation = symbolXML "-"
    toXML UOPBitNegation = symbolXML "~"