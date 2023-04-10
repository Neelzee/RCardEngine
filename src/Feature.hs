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
    deriving (Eq, Enum)


instance Show Feature where
    show f = case f of
        WinCon -> "WIN_CON"
        CardSuits -> "CARD_SUITS"
        CardValues -> "CARD_VALUES"
        CardRanks -> "CARD_RANKS"
        EndCon -> "END_CON"
        AnyTime -> "ANY_TIME"
        StartTime -> "START_TIME"
        PlayerHand -> "PLAYER_HAND"
        PlayerMoves -> "PLAYER_MOVES"
        PileCount -> "PILE_COUNT"
        GameName -> "GAME_NAME"
        Saved -> "SAVED"
        CardConstraints -> "CARD_CONSTRAINTS"


-- Checks if the given feature is valid
fromStringToFeature :: String -> Maybe Feature
fromStringToFeature x = case x of
    "WIN_CON" -> Just WinCon
    "CARD_SUITS" -> Just CardSuits
    "CARD_VALUES" -> Just CardValues
    "CARD_RANKS" -> Just CardRanks
    "END_CON" -> Just EndCon
    "ANY_TIME" -> Just AnyTime
    "PLAYER_HAND" -> Just PlayerHand
    "PLAYER_MOVES" -> Just PlayerMoves
    "PILE_COUNT" -> Just PileCount
    "GAME_NAME" -> Just GameName
    "START_TIME" -> Just StartTime
    "CARD_CONSTRAINTS" -> Just CardConstraints
    _ -> Nothing



validateKeyWords :: String -> Maybe String
validateKeyWords x = case x of
    "WIN_CON" -> Just "WIN_CON"
    "CARD_SUITS" -> Just "CARD_SUITS"
    "CARD_VALUES" -> Just "CARD_VALUES"
    "CARD_RANKS" -> Just "CARD_RANKS"
    "END_CON" -> Just "END_CON"
    "ANY_TIME" -> Just "ANY_TIME"
    "PILE_COUNT" -> Just "PILE_COUNT"
    "PLAYER_HAND" -> Just "PLAYER_HAND"
    "PLAYER_MOVES" -> Just "PLAYER_MOVES"
    "GAME_NAME" -> Just "GAME_NAME"
    "START_TIME" -> Just "START_TIME"
    "CARD_CONSTRAINTS" -> Just "CARD_CONSTRAINTS"
    _ -> Nothing
