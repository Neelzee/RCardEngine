module CardGame.Card where

import System.Random
import Functions (removeNth)
import CDSL.CDSLExpr (CDSLExpr (Text, Numeric))
import Data.List (intercalate)


data Card = Card {
    suit :: String
    , rank :: String
    , cScore :: Int
}
instance Show Card where
    show (Card s cn cv) = s ++ "." ++ cn ++ "." ++ show cv

instance Eq Card where
    (Card s1 cn1 _) == (Card s2 cn2 _) = s1 == s2 && cn1 == cn2


type Deck = [Card]

shuffle :: [a] -> [a]
shuffle xs = shuffle' xs (mkStdGen 420)


shuffle' :: RandomGen mkStdGen => [a] -> mkStdGen -> [a]
shuffle' [] _ = []
shuffle' xs gen = y : shuffle' ys gen'
  where
    (index, gen') = randomR (0, length xs - 1) gen
    (y, ys) = removeNth index xs




defaultCardSuits :: [CDSLExpr]
defaultCardSuits = [Text "Hearts", Text "Clubs", Text "Diamonds", Text "Spades"]

defaultCardRanks :: [CDSLExpr]
defaultCardRanks = [Text "Ace", Text "Two", Text "Three", Text "Four", Text "Five", Text "Six", Text "Seven", Text "Eight", Text "Nine", Text "Ten", Text "Jack", Text "Queen", Text "King"]

defaultCardValues :: [CDSLExpr]
defaultCardValues = map Numeric [1..13]

defaultCardDeck :: Deck
defaultCardDeck = [ Card s n sc | (Text s) <- defaultCardSuits, (Text n) <- defaultCardRanks, (Numeric sc) <- defaultCardValues ]

makeDeck :: [CDSLExpr] -> [CDSLExpr] -> [CDSLExpr] -> Deck
makeDeck suits ranks values =
  let paddedValues = values ++ repeat (Numeric 0)
      rankValuePairs = zipWith (curry (\(Text r, Numeric i) -> (r, i))) ranks paddedValues
      suitCards s = map (uncurry (Card s)) rankValuePairs
  in concatMap (suitCards . (\(Text s) -> s)) suits

prettyPrintCards :: [Card] -> IO ()
prettyPrintCards xs = putStrLn (intercalate ", " (map show xs))