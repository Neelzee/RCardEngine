module CardGame.Card (
    Card (..)
    , shuffle
    ) where

import System.Random.Shuffle (shuffle')
import System.Random (newStdGen)
import Functions (removeNth)



data Card = Card {
    suit :: String
    , rank :: String
    , cScore :: Int
}
    deriving (Ord)
instance Show Card where
    show (Card s cn cv) = s ++ "." ++ cn ++ "." ++ show cv

instance Eq Card where
    (Card s1 cn1 _) == (Card s2 cn2 _) = s1 == s2 && cn1 == cn2



shuffle :: [a] -> IO [a]
shuffle xs = shuffle' xs (length xs) <$> newStdGen

