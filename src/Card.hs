module Card where

data Suit = Hearts | Clubs | Diamonds | Spades
    deriving (Show, Eq)

data Value = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    deriving (Show, Eq)


data Card = Card Suit Value
    deriving (Show, Eq)

type Deck = [Card]

-- Checks if the first card can be placed ontop of the second card,
-- by checking their position in the given sorting list
canPlaceCard :: Card -> Card -> Deck -> Bool
canPlaceCard _ _ [] = True
canPlaceCard x y (c:cs) | x == c = True
    | y == c = False
    | otherwise = canPlaceCard x y cs

newDeck :: Deck
newDeck = [
    Card Hearts One,
    Card Hearts Two,
    Card Hearts Three,
    Card Hearts Four,
    Card Hearts Five,
    Card Hearts Six,
    Card Hearts Seven,
    Card Hearts Eight,
    Card Hearts Nine,
    Card Hearts Ten,
    Card Hearts Jack,
    Card Hearts Queen,
    Card Hearts King,
    Card Clubs One,
    Card Clubs Two,
    Card Clubs Three,
    Card Clubs Four,
    Card Clubs Five,
    Card Clubs Six,
    Card Clubs Seven,
    Card Clubs Eight,
    Card Clubs Nine,
    Card Clubs Ten,
    Card Clubs Jack,
    Card Clubs Queen,
    Card Clubs King,
    Card Diamonds One,
    Card Diamonds Two,
    Card Diamonds Three,
    Card Diamonds Four,
    Card Diamonds Five,
    Card Diamonds Six,
    Card Diamonds Seven,
    Card Diamonds Eight,
    Card Diamonds Nine,
    Card Diamonds Ten,
    Card Diamonds Jack,
    Card Diamonds Queen,
    Card Diamonds King,
    Card Spades One,
    Card Spades Two,
    Card Spades Three,
    Card Spades Four,
    Card Spades Five,
    Card Spades Six,
    Card Spades Seven,
    Card Spades Eight,
    Card Spades Nine,
    Card Spades Ten,
    Card Spades Jack,
    Card Spades Queen,
    Card Spades King
    ]
