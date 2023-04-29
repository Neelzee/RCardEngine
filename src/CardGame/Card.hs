module CardGame.Card (
    Card (..)
    , shuffle
    ) where

import System.Random ( mkStdGen, Random(randomR), RandomGen )
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


shuffle :: [a] -> [a]
shuffle xs = shuffle' xs (mkStdGen 420)


shuffle' :: RandomGen mkStdGen => [a] -> mkStdGen -> [a]
shuffle' [] _ = []
shuffle' xs gen = y : shuffle' ys gen'
  where
    (index, gen') = randomR (0, length xs - 1) gen
    (y, ys) = removeNth index xs
