module Card where

import System.Random
import Functions (removeNth)


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




defaultCardSuits :: [String]
defaultCardSuits = ["Hearts", "Clubs", "Diamonds", "Spades"]

defaultCardNames :: [String]
defaultCardNames = ["Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King"]

defaultCardValues :: [Int]
defaultCardValues = [1..13]

defaultCardDeck :: Deck
defaultCardDeck = [ Card s n sc | s <- defaultCardSuits, n <- defaultCardNames, sc <- defaultCardValues ]

makeDeck :: [String] -> [String] -> [Int] -> Deck
makeDeck suits ranks values =
  let paddedValues = values ++ repeat 0
      rankValuePairs = zip ranks paddedValues
      suitCards s = map (uncurry (Card s)) rankValuePairs
  in concatMap suitCards suits