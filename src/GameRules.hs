module GameRules where

data GameRule = 
    CardSuits
    | CardValues
    | CardNames
    | EndCon
    | WinCon
    | GRDeck
    | AnyTime
    | PlayerHand
    | PlayerMoves
    deriving (Show, Eq)


parseGameRules :: String -> Maybe GameRule
parseGameRules s = case s of
    "CARD_SUITS" -> Just CardSuits
    "CARD_NAMES" -> Just CardNames
    "CARD_VALUES" -> Just CardValues
    "DECK" -> Just GRDeck
    "ENDCON" -> Just EndCon
    "WINCON" -> Just WinCon
    "PLAYER_HAND" -> Just PlayerHand
    "PLAYER_MOVES" -> Just PlayerMoves
    _ -> Nothing