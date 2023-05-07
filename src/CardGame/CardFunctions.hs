module CardGame.CardFunctions (
  defaultCardDeck
  , defaultCardRanks
  , defaultCardSuits
  , defaultCardValues
  , prettyPrintCards
  , cardElem
  , makeDeck
  , prettyShowCards
  , makeCards
) where


import CDSL.CDSLExpr
import CardGame.Card
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.List.Extra (split)




defaultCardSuits :: [CDSLExpr]
defaultCardSuits = [Text "Hearts", Text "Clubs", Text "Diamonds", Text "Spades"]

defaultCardRanks :: [CDSLExpr]
defaultCardRanks = [Text "Ace", Text "Two", Text "Three", Text "Four", Text "Five", Text "Six", Text "Seven", Text "Eight", Text "Nine", Text "Ten", Text "Jack", Text "Queen", Text "King"]

defaultCardValues :: [CDSLExpr]
defaultCardValues = map Numeric [1..13]

defaultCardDeck :: [Card]
defaultCardDeck = [ Card s n sc | (Text s) <- defaultCardSuits, (Text n) <- defaultCardRanks, (Numeric sc) <- defaultCardValues ]

-- Due to prev. validation, there will only be Text and Numeric expressions here
makeDeck :: [CDSLExpr] -> [CDSLExpr] -> [CDSLExpr] -> [Card]
makeDeck suits ranks values =
  let paddedValues = values ++ repeat (Numeric 0)
      rankValuePairs = zipWith (curry (\(Text r, Numeric i) -> (r, i))) ranks paddedValues
      suitCards s = map (uncurry (Card s)) rankValuePairs
  in concatMap (suitCards . (\(Text s) -> s)) suits

prettyPrintCards :: [Card] -> IO ()
prettyPrintCards xs = putStrLn (intercalate ", " (map show xs))

prettyShowCards :: [Card] -> String
prettyShowCards xs = intercalate ", " (map (\(Card s r _) -> s ++ "." ++ r) xs)


cardElem :: Card -> [Card] -> Bool
cardElem _ [] = False
cardElem c@(Card s r _) ((Card ss rr _):xs)
    | (ss == "" || s == ss) && (rr == "" || rr == r) = True
    | otherwise = cardElem c xs


makeCards :: [CDSLExpr] -> CDSLExpr
makeCards ex = Cards (mapMaybe go ex)
  where
    go ((Text str)) = case split (==',') str of
      [s, r, _] -> Just (Card s r 0)
      [s, r] -> Just (Card s r 0)
      _ -> Nothing
    go _ = Nothing
