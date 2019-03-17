module Data.AnalyserModel where

import qualified Data.ByteString.Char8 as BS


newtype EIntegerConstant = EIntegerConstant Integer
newtype EStringConstant  = EStringConstant BS.ByteString

data EKeywordConstant =
    EKConTrue
  | EKConFalse
  | EKConNull
  | EKConThis

newtype EVarName = EVarName BS.ByteString 

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


