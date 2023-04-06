module Feature where


data Feature = WinCon
    | CardSuits
    | CardValues
    | CardRanks
    | EndCon
    | AnyTime
    | StartTime
    | PlayerHand
    | PlayerMoves
    | PileCount
    | GameName
    | Saved
    | CardConstraints
    deriving (Show, Eq, Enum)


-- Checks if the given feature is valid
fromStringToFeature :: String -> Maybe Feature
fromStringToFeature x = case x of
    "WINCON" -> Just WinCon
    "CARD_SUITS" -> Just CardSuits
    "CARD_VALUES" -> Just CardValues
    "CARD_RANKS" -> Just CardRanks
    "ENDCON" -> Just EndCon
    "ANYTIME" -> Just AnyTime
    "PLAYER_HAND" -> Just PlayerHand
    "PLAYER_MOVES" -> Just PlayerMoves
    "PILE_COUNT" -> Just PileCount
    "GAMENAME" -> Just GameName
    "STARTTIME" -> Just StartTime
    _ -> Nothing



validateKeyWords :: String -> Maybe String
validateKeyWords x = case x of
    "WIN_CON" -> Just "WIN_CON"
    "CARD_SUITS" -> Just "CARD_SUITS"
    "CARD_VALUES" -> Just "CARD_VALUES"
    "CARD_RANKS" -> Just "CARD_RANKS"
    "END_CON" -> Just "END_CON"
    "ANYTIME" -> Just "ANYTIME"
    "PILE_COUNT" -> Just "PILE_COUNT"
    "PLAYER_HAND" -> Just "PLAYER_HAND"
    "PLAYER_MOVES" -> Just "PLAYER_MOVES"
    "GAME_NAME" -> Just "GAME_NAME"
    "STAR_TTIME" -> Just "START_TIME"
    _ -> Nothing