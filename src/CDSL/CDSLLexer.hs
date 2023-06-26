module CDSL.CDSLLexer where

data CDSLLexer =
  KeyWordToken KeyWord
  | TypeToken Type
  | DelimiterStartToken Delimiter
  | DelimiterEndToken Delimiter
  | SymbolToken String
  | AssignToken
  | BoolOperatorToken BoolOperator
  | NumericOperatorToken NumericOperator
  | NumericToken Int
  | EndToken
  deriving (Eq, Show)


data Type = 
  Number 
  | Boolean
  | Card
  | Player
  | List Type
  deriving (Eq, Show)

data KeyWord =
  For
  | If
  | Else
  | Continue
  | Break
  | Return
  deriving (Eq, Show)

data Delimiter =
  Parentheses
  | CurlyBrackets
  | SquareBrackets
  deriving (Eq, Show)


data BoolOperator =
  -- ==
  Eq
  -- !=
  | Neq
  -- <
  | Lt
  -- >
  | Gt
  -- <=
  | Leq
  -- >=
  | Geq
  -- &&
  | And
  -- ||
  | Or
  deriving (Eq, Show)

data NumericOperator =
  -- +
  Add
  -- -
  | Sub
  -- *
  | Mul
  -- /
  | DivUp
  -- \
  | DivDown
  -- %
  | Mod
  -- ^
  | Pow
  deriving (Eq, Show)


