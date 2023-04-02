module Card where

data Card = Card {
    suit :: String
    , cName :: String
    , cScore :: Int
}
instance Show Card where
    show (Card s cn cv) = s ++ "." ++ cn ++ "." ++ show cv

instance Eq Card where
    (Card s1 cn1 _) == (Card s2 cn2 _) = s1 == s2 && cn1 == cn2


type Deck = [Card]

-- Checks if the first card can be placed ontop of the second card,
-- by checking their position in the given sorting list
canPlaceCard :: Card -> Card -> Deck -> Bool
canPlaceCard _ _ [] = True
canPlaceCard x y (c:cs) | x == c = True
    | y == c = False
    | otherwise = canPlaceCard x y cs


defaultCardSuits :: [String]
defaultCardSuits = ["Hearts", "Clubs", "Diamonds", "Spades"]

defaultCardNames :: [String]
defaultCardNames = ["Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King"]

defaultCardDeck :: Deck
defaultCardDeck = [
    Card "Hearts" "Ace" 0,
    Card "Hearts" "Two" 0,
    Card "Hearts" "Three" 0,
    Card "Hearts" "Four" 0,
    Card "Hearts" "Five" 0,
    Card "Hearts" "Six" 0,
    Card "Hearts" "Seven" 0,
    Card "Hearts" "Eight" 0,
    Card "Hearts" "Nine" 0,
    Card "Hearts" "Ten" 0,
    Card "Hearts" "Jack" 0,
    Card "Hearts" "Queen" 0,
    Card "Hearts" "King" 0,
    Card "Clubs" "Ace" 0,
    Card "Clubs" "Two" 0,
    Card "Clubs" "Three" 0,
    Card "Clubs" "Four" 0,
    Card "Clubs" "Five" 0,
    Card "Clubs" "Six" 0,
    Card "Clubs" "Seven" 0,
    Card "Clubs" "Eight" 0,
    Card "Clubs" "Nine" 0,
    Card "Clubs" "Ten" 0,
    Card "Clubs" "Jack" 0,
    Card "Clubs" "Queen" 0,
    Card "Clubs" "King" 0,
    Card "Diamonds" "Ace" 0,
    Card "Diamonds" "Two" 0,
    Card "Diamonds" "Three" 0,
    Card "Diamonds" "Four" 0,
    Card "Diamonds" "Five" 0,
    Card "Diamonds" "Six" 0,
    Card "Diamonds" "Seven" 0,
    Card "Diamonds" "Eight" 0,
    Card "Diamonds" "Nine" 0,
    Card "Diamonds" "Ten" 0,
    Card "Diamonds" "Jack" 0,
    Card "Diamonds" "Queen" 0,
    Card "Diamonds" "King" 0,
    Card "Spades" "Ace" 0,
    Card "Spades" "Two" 0,
    Card "Spades" "Three" 0,
    Card "Spades" "Four" 0,
    Card "Spades" "Five" 0,
    Card "Spades" "Six" 0,
    Card "Spades" "Seven" 0,
    Card "Spades" "Eight" 0,
    Card "Spades" "Nine" 0,
    Card "Spades" "Ten" 0,
    Card "Spades" "Jack" 0,
    Card "Spades" "Queen" 0,
    Card "Spades" "King" 0
    ]
