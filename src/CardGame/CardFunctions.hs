module CardGame.CardFunctions (
  defaultCardDeck
  , defaultCardRanks
  , defaultCardSuits
  , defaultCardValues
  , prettyPrintCards
  , cardElem
  , makeDeck
  , prettyShowCards
) where


import CDSL.CDSLExpr
import CardGame.Card
import Data.List (intercalate)




defaultCardSuits :: [CDSLExpr]
defaultCardSuits = [Text "Hearts", Text "Clubs", Text "Diamonds", Text "Spades"]

defaultCardRanks :: [CDSLExpr]
defaultCardRanks = [Text "Ace", Text "Two", Text "Three", Text "Four", Text "Five", Text "Six", Text "Seven", Text "Eight", Text "Nine", Text "Ten", Text "Jack", Text "Queen", Text "King"]

defaultCardValues :: [CDSLExpr]
defaultCardValues = map Numeric [1..13]

defaultCardDeck :: [Card]
defaultCardDeck = [ Card s n sc | (Text s) <- defaultCardSuits, (Text n) <- defaultCardRanks, (Numeric sc) <- defaultCardValues ]

makeDeck :: [CDSLExpr] -> [CDSLExpr] -> [CDSLExpr] -> [Card]
makeDeck suits ranks values =
  let paddedValues = values ++ repeat (Numeric 0)
      rankValuePairs = zipWith (curry (\(Text r, Numeric i) -> (r, i))) ranks paddedValues
      suitCards s = map (uncurry (Card s)) rankValuePairs
  in concatMap (suitCards . (\(Text s) -> s)) suits

prettyPrintCards :: [Card] -> IO ()
prettyPrintCards xs = putStrLn (intercalate ", " (map show xs))

prettyShowCards :: [Card] -> String
prettyShowCards xs = intercalate ", " (map show xs)
cardElem :: Card -> [Card] -> Bool
cardElem _ [] = False
cardElem c@(Card s r _) ((Card ss rr _):xs)
    | (ss == "" || s == ss) && (rr == "" || rr == r) = True
    | otherwise = cardElem c xs
