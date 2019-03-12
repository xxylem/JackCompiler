module Model where

data Keyword =
    Class
  | Constructor
  | Function
  | Method
  | Field
  | Static
  | Var
  | Int
  | Char
  | Boolean
  | Void
  | TrueKW
  | FalseKW
  | Null
  | This
  | Let
  | Do 
  | If
  | Else
  | While
  | Return

data Symbol =
    LCurlyBracket
  | RCurlyBracket
  | LParen
  | RParen
  | LSquareBracket
  | RSquareBracket
  | FullStop
  | Comma
  | Semicolon
  | Plus
  | Minus
  | Asterix
  | Slash
  | Ampersand
  | VerticalBar
  | LAngleBracket
  | RAngleBracket
  | Equal
  | Tilde

type IntegerConstant = Integer
type StringConstant = String
type Identifier = String

data Token =
    KW Keyword
  | SY Symbol
  | IC IntegerConstant
  | SC StringConstant
  | ID Identifier