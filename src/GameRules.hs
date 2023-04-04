module GameRules where

data GameRule = 
    CardSuits
    | CardValues
    | CardNames
    | EndCon
    | WinCon
    | GRDeck
    | AnyTime
    | StartTime
    | PlayerHand
    | PileCount
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
    "ANYTIME" -> Just AnyTime
    "PLAYER_MOVES" -> Just PlayerMoves
    "STARTTIME" -> Just StartTime
    _ -> Nothing


isKeyword :: String -> Bool
isKeyword str = case parseGameRules str of
    Just _ -> True
    Nothing -> False