module CDSL.CDSLAST where


import CDSL.CDSLLexer
  (
    CDSLLexer (..),
    Type (..),
    KeyWord (..),
    Delimiter (..),
    BoolOperator (..),
    NumericOperator (..)
  )


data Node = Node {
    token :: CDSLLexer
    , right :: Maybe Node
    , left :: Maybe Node
  }




buildTree :: [CDSLLexer] -> Maybe [Node]
buildTree (x:xs) = buildTree' (Node x Nothing Nothing) xs


buildTree' :: Node -> [CDSLLexer] -> Maybe [Node]
-- If the node is a KeyWord
buildTree' n@(Node t@(KeyWordToken k) r l) (x:xs) = case x of
  SymbolToken t -> case (k, l) of
    (Return, Nothing) -> buildTree' (n { left = Just (Node x (Just n) Nothing) }) xs
    _ -> Nothing -- Only works if the left node is open
  -- Start of a delimiter
  DelimiterStartToken t -> case (k, l, t) of
    (Break, _, _) -> Nothing
    (Continue, _, _) -> Nothing
    (_, Nothing, Parentheses) -> buildTree' (n { left = Just (Node x (Just n) Nothing) }) xs
    _ -> Nothing
  BoolOperatorToken t -> case (k, l) of
    (Return, Nothing) -> buildTree' (n { left = Just (Node x (Just n) Nothing) }) xs
    _ -> Nothing -- Only works if the left node is open
  NumericToken t -> case (k, l) of
    (Return, Nothing) -> buildTree' (n { left = Just (Node x (Just n) Nothing) }) xs
    _ -> Nothing -- Only works if the left node is open
  EndToken -> Nothing
  AssignToken -> case (k, l) of
    (Return, Nothing) -> buildTree' (n { left = Just (Node x (Just n) Nothing) }) xs
    _ -> Nothing -- Only works if the left node is open
  EndToken -> case (k, l) of
    (Return, Nothing) -> buildTree' (n { left = Just (Node x (Just n) Nothing) }) xs
    _ -> Nothing -- Only works if the left node is open
  
buildTree' n@(Node t@(TypeToken _) r l) (x:xs) = case x of
  SymbolToken t -> case (r, l) of
    (_, Nothing) -> buildTree' (n { left = Just (Node x (Just n) Nothing) }) xs
    _ -> Nothing -- Only works if the left node is open
  _ -> Nothing
  
buildTree' n@(Node t@(SymbolToken _) r l) (x:xs) = case x of
  AssignToken -> buildTree' (n { left = Just (Node x (Just n) Nothing) }) xs

buildTree' n@(Node t@(NumericToken _) r l) (x:xs) = case x of
  EndToken -> case buildTree' (Node x Nothing Nothing) xs of
    Just xs -> Just $ n { right = Just (Node x (Just n) Nothing) } : xs 
    Nothing -> Nothing


