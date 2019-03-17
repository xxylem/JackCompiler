module Data.AnalyserModel where

import qualified Data.ByteString.Char8 as BS

data Expression =
    ESingleTerm Term
  | ETerms      Term Op Term

data Term =
    TIC EIntegerConstant
  | TSC EStringConstant
  | TKC EKeywordConstant
  | TVN VarName
  | TAR EArrayExp
  | TSR SubroutineCall

newtype EIntegerConstant = EIntegerConstant Integer
newtype EStringConstant  = EStringConstant BS.ByteString
newtype VarName = VarName BS.ByteString

data EKeywordConstant =
    EKConTrue
  | EKConFalse
  | EKConNull
  | EKConThis

data EArrayExp =
    EArrayExp VarName Expression

newtype SubroutineName = SubroutineName BS.ByteString
newtype ClassName = ClassName BS.ByteString

data SubroutineCall =
    SR      SubroutineName [Expression]
  | SRCN    ClassName SubroutineName [Expression]
  | SRVN    VarName SubroutineName [Expression]

newtype ParenExpression = ParenExpression Expression

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

data UnaryOp =
    UOPArithNegation
  | UOPBitNegation
