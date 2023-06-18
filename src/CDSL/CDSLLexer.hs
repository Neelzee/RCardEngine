module CDSL.CDSLLexer where

data CDSLLexer =
  KeyWordToken KeyWord
  | TypeToken Type
  | DelimiterStartToken Delimiter
  | DelimiterEndToken Delimiter
  | SymbolToken String
  | AssignToken
  | BoolOperatorToken BoolOperator
  | NumericToken Int
  | EndToken


data Type = 
  Decimal
  | Boolean
  | Card
  | Player
  | List Type


data KeyWord =
  For
  | If
  | Else
  | Continue
  | Break
  | Return

data Delimiter =
  Parentheses
  | CurlyBrackets
  | SquareBrackets


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
  | DiwDown
  -- %
  | Mod
  -- ^
  | Pow


