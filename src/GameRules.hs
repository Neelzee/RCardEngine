module GameRules where

data GameRule = 
    CardSuits
    | CardValues
    | CardRanks
    | EndCon
    | WinCon
    | GRDeck
    | AnyTime
    | StartTime
    | PlayerHand
    | PileCount
    | PlayerMoves
    | CardConstraints
    deriving (Show, Eq)


parseGameRules :: String -> Maybe GameRule
parseGameRules s = case s of
    "CARD_SUITS" -> Just CardSuits
    "CARD_RANKS" -> Just CardRanks
    "CARD_VALUES" -> Just CardValues
    "DECK" -> Just GRDeck
    "ENDCON" -> Just EndCon
    "WINCON" -> Just WinCon
    "PLAYER_HAND" -> Just PlayerHand
    "ANYTIME" -> Just AnyTime
    "PLAYER_MOVES" -> Just PlayerMoves
    "STARTTIME" -> Just StartTime
    "CARD_CONSTRAINTS" -> Just CardConstraints
    _ -> Nothing


isKeyword :: String -> Bool
isKeyword str = case parseGameRules str of
    Just _ -> True
    Nothing -> False