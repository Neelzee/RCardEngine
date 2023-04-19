module CardGame.Card (
    CardEffect (..)
    , Card (..)
    , shuffle
    ) where

import System.Random
import Functions (removeNth)


data CardEffect =
    -- Choose a new card to change to, i.e. standard "Vri-Åttern"
    ChangeCard
    -- Swap hand with another Player
    | SwapHand
    -- Takes a Card from an Player's Hand
    | TakeFromHand
    -- Gives a Card too another Player
    | GiveCard
    -- Passes the next Player's round
    | PassNext
    -- Makes the Player draw the given amount of cards, and skip their turn
    | DrawCards Int
    deriving (Show, Eq)


data Card = Card {
    suit :: String
    , rank :: String
    , cScore :: Int
}
instance Show Card where
    show (Card s cn cv) = s ++ "." ++ cn ++ "." ++ show cv

instance Eq Card where
    (Card s1 cn1 _) == (Card s2 cn2 _) = s1 == s2 && cn1 == cn2


shuffle :: [a] -> [a]
shuffle xs = shuffle' xs (mkStdGen 420)


shuffle' :: RandomGen mkStdGen => [a] -> mkStdGen -> [a]
shuffle' [] _ = []
shuffle' xs gen = y : shuffle' ys gen'
  where
    (index, gen') = randomR (0, length xs - 1) gen
    (y, ys) = removeNth index xs
