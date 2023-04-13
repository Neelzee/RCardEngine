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
    | CardFeatures
    | PlayerFeatures
    | Actions
    | GameFeatures
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
        CardFeatures -> "CARD"
        PlayerFeatures -> "PLAYER"
        Actions -> "ACTIONS"
        GameFeatures -> "GAME"


-- Checks if the given feature is valid
fromStringToFeature :: String -> Maybe Feature
fromStringToFeature x = case x of
    "win_con" -> Just WinCon
    "CARD_SUITS" -> Just CardSuits
    "CARD_VALUES" -> Just CardValues
    "CARD_RANKS" -> Just CardRanks
    "end_con" -> Just EndCon
    "ANY_TIME" -> Just AnyTime
    "PLAYER_HAND" -> Just PlayerHand
    "PLAYER_MOVES" -> Just PlayerMoves
    "PILE_COUNT" -> Just PileCount
    "GAME_NAME" -> Just GameName
    "START_TIME" -> Just StartTime
    "CARD_CONSTRAINTS" -> Just CardConstraints
    "CARD" -> Just CardFeatures
    "PLAYER" -> Just PlayerFeatures
    "ACTIONS" -> Just Actions
    "GAME" -> Just GameFeatures
    _ -> Nothing



validateKeyWords :: String -> Maybe String
validateKeyWords x = case x of
    "win_con" -> Just "win_con"
    "CARD_SUITS" -> Just "CARD_SUITS"
    "CARD_VALUES" -> Just "CARD_VALUES"
    "CARD_RANKS" -> Just "CARD_RANKS"
    "end_con" -> Just "end_con"
    "ANY_TIME" -> Just "ANY_TIME"
    "PILE_COUNT" -> Just "PILE_COUNT"
    "PLAYER_HAND" -> Just "PLAYER_HAND"
    "PLAYER_MOVES" -> Just "PLAYER_MOVES"
    "GAME_NAME" -> Just "GAME_NAME"
    "START_TIME" -> Just "START_TIME"
    "CARD_CONSTRAINTS" -> Just "CARD_CONSTRAINTS"
    "CARD" -> Just "CARD"
    "PLAYER" -> Just "PLAYER"
    "ACTIONS" -> Just "ACTIONS"
    "GAME" -> Just "GAME"
    _ -> Nothing


isAFeatureOf :: Feature -> Feature -> Bool
isAFeatureOf CardConstraints CardFeatures = True
isAFeatureOf CardSuits CardFeatures = True
isAFeatureOf CardRanks CardFeatures = True

isAFeatureOf CardValues CardFeatures = True
isAFeatureOf PlayerHand PlayerFeatures = True
isAFeatureOf PlayerMoves PlayerFeatures = True

isAFeatureOf AnyTime Actions = True
isAFeatureOf StartTime Actions = True

isAFeatureOf WinCon GameFeatures = True
isAFeatureOf EndCon GameFeatures = True

isAFeatureOf _ _ = False