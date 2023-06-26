module CDSL.CDSLTokenizer where


import CDSL.CDSLLexer
import Text.Read (readMaybe)



parseToTokens :: [String] -> [CDSLLexer]
parseToTokens [] = []
parseToTokens (x:xs) = case x of
  "for" -> KeyWordToken For : parseToTokens xs
  "if" -> KeyWordToken If : parseToTokens xs
  "else" -> KeyWordToken Else : parseToTokens xs
  "continue" -> KeyWordToken Continue : parseToTokens xs
  "break" -> KeyWordToken Break : parseToTokens xs
  "return" -> KeyWordToken Return : parseToTokens xs
  "(" -> DelimiterStartToken Parentheses : parseToTokens xs
  "[" -> DelimiterStartToken SquareBrackets : parseToTokens xs
  "{" -> DelimiterStartToken CurlyBrackets : parseToTokens xs
  ")" -> DelimiterEndToken Parentheses : parseToTokens xs
  "]" -> DelimiterEndToken SquareBrackets : parseToTokens xs
  "}" -> DelimiterEndToken CurlyBrackets : parseToTokens xs
  "=" -> AssignToken : parseToTokens xs
  "==" -> BoolOperatorToken Eq : parseToTokens xs
  "!=" -> BoolOperatorToken Neq : parseToTokens xs
  ">" -> BoolOperatorToken Gt : parseToTokens xs
  "<" -> BoolOperatorToken Lt : parseToTokens xs
  ">=" -> BoolOperatorToken Geq : parseToTokens xs
  "<=" -> BoolOperatorToken Leq : parseToTokens xs
  "&&" -> BoolOperatorToken And : parseToTokens xs
  "||" -> BoolOperatorToken Or : parseToTokens xs
  "+" -> NumericOperatorToken Add : parseToTokens xs
  "-" -> NumericOperatorToken Sub : parseToTokens xs
  "*" -> NumericOperatorToken Mul : parseToTokens xs
  "/" -> NumericOperatorToken DivUp : parseToTokens xs
  "\\" -> NumericOperatorToken DivDown : parseToTokens xs
  "%" -> NumericOperatorToken Mod : parseToTokens xs
  "^" -> NumericOperatorToken Pow : parseToTokens xs
  "int" -> TypeToken Number : parseToTokens xs
  "bool" -> TypeToken Boolean : parseToTokens xs
  "Card" -> TypeToken Card : parseToTokens xs
  "Player" -> TypeToken Player : parseToTokens xs
  ";" -> EndToken : parseToTokens xs
  _ -> case readMaybe x :: Maybe Int of
    Just i -> NumericToken i : parseToTokens xs
    Nothing -> if isList x
      then
        SymbolToken x : parseToTokens xs
      else
        SymbolToken x : parseToTokens xs

isList :: String -> Bool
isList _ = True
