module Data.AnalyserModel where

import qualified Data.ByteString.Char8 as BS

data VarDec =
    VarDec JackType [VarName]

data JackType =
    IntType
  | CharType
  | BoolType
  | ClassType ClassName

data Statement =
    LetStatement LetStatementName Expression
  | IfStatement Expression [Statement] (Maybe [Statement])
  | WhileStatement Expression [Statement]
  | DoStatement SubroutineCall
  | ReturnStatement (Maybe Expression)

data LetStatementName =
    LSV VarName
  | LSA EArrayExp

data Expression =
    ESingleTerm Term
  | ETermOpTerm      Term Op Term
  deriving (Eq, Show)

data Term =
    TIntegerConstant Integer
  | TStringConstant BS.ByteString
  | TKeywordConstant EKeywordConstant
  | TVarName VarName
  | TArrayExp EArrayExp
  | TSubroutineCall SubroutineCall
  | TParenExpression Expression
  | TUnaryOp UnaryOp Term
  deriving (Eq, Show)

newtype VarName = VarName BS.ByteString
    deriving (Eq, Show)

data EKeywordConstant =
    EKConTrue
  | EKConFalse
  | EKConNull
  | EKConThis
  deriving (Eq, Show)

data EArrayExp =
    EArrayExp VarName Expression
    deriving (Eq, Show)

newtype SubroutineName = SubroutineName BS.ByteString
    deriving (Eq, Show)
newtype ClassName = ClassName BS.ByteString
    deriving (Eq, Show)

data SubroutineCall =
    SR      SubroutineName [Expression]
  | SRCN    ClassName SubroutineName [Expression]
  | SRVN    VarName SubroutineName [Expression]
  deriving (Eq, Show)

data Op =
    OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpAnd
  | OpOr
  | OpLT
  | OpGT
  | OpEqual
  deriving (Eq, Show)

data UnaryOp =
    UOPArithNegation
  | UOPBitNegation
  deriving (Eq, Show)
