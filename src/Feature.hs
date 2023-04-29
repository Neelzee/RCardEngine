module Feature (
    Feature (..)
    , Attribute (..)
    , fromStringToFeature
    , validateKeyWords
    , isAFeatureOf
    , featureInfo
    , validateAttribute
) where

data Attribute =
    GameAttributes
    | CardAttributes
    | PlayerAttributes
    | Actions
    | None
    deriving (Show, Eq)

data Feature = WinCon
    | CardSuits
    | CardValues
    | CardRanks
    | EndCon
    | AnyTime
    | StartTime
    | TurnStartTime
    | TurnEndTime
    | PlayerHand
    | PlayerMoves
    | PileCount
    | GameName
    | Saved
    | CardConstraints
    | IgnoreConstraints
    | ExceptionConstraints
    | CardFeatures
    | PlayerFeatures
    | GameFeatures
    | CardEffects
    | CEChangeCard
    | CESwapHand
    | CETakeFromHand
    | CEGiveCard
    | CEPassNext
    | CEDrawCard
    | CardCompare
    | TurnOrder
    deriving (Eq)


instance Show Feature where
    show f = case f of
        WinCon -> "WIN_CON"
        CardSuits -> "CARD_SUITS"
        CardValues -> "CARD_VALUES"
        CardRanks -> "CARD_RANKS"
        EndCon -> "END_CON"
        AnyTime -> "any_time"
        StartTime -> "start_time"
        TurnEndTime -> "turn_end"
        TurnStartTime -> "turn_start"
        PlayerHand -> "player_hand"
        PlayerMoves -> "player_moves"
        PileCount -> "PILE_COUNT"
        GameName -> "GAME_NAME"
        Saved -> "SAVED"
        CardConstraints -> "CARD_CONSTRAINTS"
        CardFeatures -> "CARD"
        PlayerFeatures -> "PLAYER"
        GameFeatures -> "GAME"
        CardEffects -> "CARD_EFFECTS"
        CEChangeCard -> "change_card"
        CESwapHand -> "swap_hand"
        CETakeFromHand -> "take_from_hand"
        CEGiveCard -> "give_card"
        CEPassNext -> "pass_next"
        CEDrawCard -> "draw_card"
        IgnoreConstraints -> "IGNORE_CONSTRAINTS"
        CardCompare -> "CARD_COMPARE"
        ExceptionConstraints -> "EXCEPTION_CONSTRAINTS"
        TurnOrder -> "TURN_ORDER"


-- Checks if the given feature is valid
fromStringToFeature :: String -> Maybe Feature
fromStringToFeature x = case x of
    "win_con" -> Just WinCon
    "card_suits" -> Just CardSuits
    "card_values" -> Just CardValues
    "card_ranks" -> Just CardRanks
    "end_con" -> Just EndCon
    "any_time" -> Just AnyTime
    "player_hand" -> Just PlayerHand
    "player_moves" -> Just PlayerMoves
    "PILE_COUNT" -> Just PileCount
    "GAME_NAME" -> Just GameName
    "start_time" -> Just StartTime
    "card_constraints" -> Just CardConstraints
    "CARD" -> Just CardFeatures
    "PLAYER" -> Just PlayerFeatures
    "GAME" -> Just GameFeatures
    "CARD_EFFECTS" -> Just CardEffects
    "change_card" -> Just CEChangeCard
    "swap_hand" -> Just CESwapHand
    "take_from_hand" -> Just CETakeFromHand
    "give_card" -> Just CEGiveCard
    "pass_next" -> Just CEPassNext
    "draw_card" -> Just CEDrawCard
    "turn_start" -> Just TurnStartTime
    "turn_end" -> Just TurnEndTime
    "ignore_constraints" -> Just IgnoreConstraints
    "card_compare" -> Just CardCompare
    "exception_constraints" -> Just ExceptionConstraints
    "turn_order" -> Just TurnOrder
    _ -> Nothing



validateKeyWords :: String -> Maybe String
validateKeyWords x = case x of
    "win_con" -> Just "win_con"
    "card_suits" -> Just "card_suits"
    "card_values" -> Just "card_values"
    "card_ranks" -> Just "card_ranks"
    "end_con" -> Just "end_con"
    "any_time" -> Just "any_time"
    "PILE_COUNT" -> Just "PILE_COUNT"
    "player_hand" -> Just "player_hand"
    "player_moves" -> Just "player_moves"
    "GAME_NAME" -> Just "GAME_NAME"
    "start_time" -> Just "start_time"
    "card_constraints" -> Just "card_constraints"
    "CARD" -> Just "CARD"
    "PLAYER" -> Just "PLAYER"
    "ACTIONS" -> Just "ACTIONS"
    "GAME" -> Just "GAME"
    "CARD_EFFECTS" -> Just "CARD_EFFECTS"
    "change_card" -> Just "change_card"
    "swap_hand" -> Just "swap_hand"
    "take_from_hand" -> Just "take_from_hand"
    "give_card" -> Just "give_card"
    "pass_next" -> Just "pass_next"
    "draw_card" -> Just "draw_card"
    "turn_start" -> Just "turn_start"
    "turn_end" -> Just "turn_end"
    "ignore_constraints" -> Just "ignore_constraints"
    "card_compare" -> Just "card_compare"
    "exception_constraints" -> Just "exception_constraints"
    "turn_order" -> Just "turn_order"
    _ -> Nothing


isAFeatureOf :: Feature -> Attribute -> Bool
isAFeatureOf CEChangeCard CardAttributes = True
isAFeatureOf CEDrawCard CardAttributes = True
isAFeatureOf CEGiveCard CardAttributes = True
isAFeatureOf CEPassNext CardAttributes = True
isAFeatureOf CETakeFromHand CardAttributes = True
isAFeatureOf CESwapHand CardAttributes = True
isAFeatureOf CardEffects CardAttributes = True

isAFeatureOf CardConstraints CardAttributes = True
isAFeatureOf CardSuits CardAttributes = True
isAFeatureOf CardRanks CardAttributes = True
isAFeatureOf IgnoreConstraints CardAttributes = True
isAFeatureOf CardCompare CardAttributes = True
isAFeatureOf CardValues CardAttributes = True
isAFeatureOf ExceptionConstraints CardAttributes = True

isAFeatureOf PlayerHand PlayerAttributes = True
isAFeatureOf PlayerMoves PlayerAttributes = True

isAFeatureOf AnyTime Actions = True
isAFeatureOf StartTime Actions = True
isAFeatureOf TurnStartTime Actions = True
isAFeatureOf TurnEndTime Actions = True

isAFeatureOf WinCon GameAttributes = True
isAFeatureOf EndCon GameAttributes = True
isAFeatureOf TurnOrder GameAttributes = True

isAFeatureOf _ _ = False


featureInfo :: [(Feature, String)]
featureInfo = [
    (WinCon, "Win Condition")
    , (CardSuits, "Card Suits")
    , (CardRanks, "Card Ranks")
    , (CardValues, "Card Values, defaults too 0 if not enough")
    , (EndCon, "End Condition")
    , (AnyTime, "Expressions that will be executed AnyTime, i.e, whenever its possible")
    , (StartTime, "Expressions that are executed once, at Start Time, before a player makes their move")
    , (TurnStartTime, "Expressions that will be executed at the start of a players turn")
    , (TurnEndTime, "Expressions that will be executed at the en of a players turn")
    , (PlayerHand, "Specifies how many cards a player starts with")
    , (PlayerMoves, "Specifies the valid moves a player can make")
    , (PileCount, "specifies how many cards should be places in the pile at start up")
    , (CardConstraints, "Specifies what field will be used in the comparision")
    , (IgnoreConstraints, "A list of cards that will ignore the normal constraints")
    , (CEChangeCard, "Specifies that the given cards will trigger a change card effect")
    , (CESwapHand, "Specifies that the given cards will trigger a swap hand effect")
    , (CETakeFromHand, "Specifies that the given cards will trigger a take from hand effect")
    , (CEGiveCard, "Specifies that the given cards will trigger a give card effect")
    , (CEPassNext, "Specifies that the given cards will trigger a pass effect")
    , (CEDrawCard, "Specifies that the given cards will trigger a draw card effect")
    , (CardCompare, "What comparator will be used to compare cards")
    , (TurnOrder, "Used to specify what ordering will be used")
    ]



validateAttribute :: String -> Maybe Attribute
validateAttribute x = case x of
    "GAME" -> Just GameAttributes
    "CARD" -> Just CardAttributes
    "PLAYER" -> Just PlayerAttributes
    "ACTIONS" -> Just Actions
    "CARD_EFFECTS" -> Just CardAttributes
    _ -> Nothing
