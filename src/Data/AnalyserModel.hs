module Data.AnalyserModel where

import qualified Data.ByteString.Char8 as BS

data Expression =
    ESingleTerm Term
  | ETerms      Term Op Term

data Term =
    TIC EIntegerConstant
  | TSC EStringConstant
  | TKC EKeywordConstant
  | TVN EVarName


newtype EIntegerConstant = EIntegerConstant Integer
newtype EStringConstant  = EStringConstant BS.ByteString

data EKeywordConstant =
    EKConTrue
  | EKConFalse
  | EKConNull
  | EKConThis

newtype EVarName = EVarName BS.ByteString

data EArrayExp =
    EArrayExp EVarName Expression

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
