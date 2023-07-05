#[derive(Clone, Copy, Debug)]
pub enum Attribute {
    GameAttributes,
    CardAttributes,
    PlayerAttributes,
    Actions,
    None
}

#[derive(Clone, Copy, Debug)]
pub enum Feature {
    WinCon,
    CardSuits,
    CardValues,
    CardRanks,
    EndCon,
    AnyTime,
    StartTime,
    TurnStartTime,
    TurnEndTime,
    PlayerHand,
    PlayerMoves,
    PileCount,
    GameName,
    CardConstraints,
    IgnoreConstraints,
    ExceptionConstraints,
    CardFeatures,
    PlayerFeatures,
    GameFeatures,
    CEChangeCard,
    CESwapHand,
    CETakeFromHand,
    CEGiveCard,
    CEPassNext,
    CEDrawCard,
    CardCompare,
    TurnOrder
}


impl Feature {
    pub fn from_string(x: &str) -> Option<Feature> {
        match x {
            "win_con" => Some(Feature::WinCon),
            "card_suits" => Some(Feature::CardSuits),
            "card_values" => Some(Feature::CardValues),
            "card_ranks" => Some(Feature::CardRanks),
            "end_con" => Some(Feature::EndCon),
            "any_time" => Some(Feature::AnyTime),
            "player_hand" => Some(Feature::PlayerHand),
            "player_moves" => Some(Feature::PlayerMoves),
            "PILE_COUNT" => Some(Feature::PileCount),
            "GAME_NAME" => Some(Feature::GameName),
            "start_time" => Some(Feature::StartTime),
            "card_constraints" => Some(Feature::CardConstraints),
            "CARD" => Some(Feature::CardFeatures),
            "PLAYER" => Some(Feature::PlayerFeatures),
            "GAME" => Some(Feature::GameFeatures),
            "change_card" => Some(Feature::CEChangeCard),
            "swap_hand" => Some(Feature::CESwapHand),
            "take_from_hand" => Some(Feature::CETakeFromHand),
            "give_card" => Some(Feature::CEGiveCard),
            "pass_next" => Some(Feature::CEPassNext),
            "draw_card" => Some(Feature::CEDrawCard),
            "turn_start" => Some(Feature::TurnStartTime),
            "turn_end" => Some(Feature::TurnEndTime),
            "ignore_constraints" => Some(Feature::IgnoreConstraints),
            "card_compare" => Some(Feature::CardCompare),
            "exception_constraints" => Some(Feature::ExceptionConstraints),
            "turn_order" => Some(Feature::TurnOrder),
            _ => None
        }
    }

    pub fn is_feature_of(&self, att: &Attribute) -> bool {
        match (self, att) {
            (Feature::CEChangeCard, Attribute::CardAttributes) => true,
            (Feature::CEDrawCard, Attribute::CardAttributes) => true,
            (Feature::CEGiveCard, Attribute::CardAttributes) => true,
            (Feature::CEPassNext, Attribute::CardAttributes) => true,
            (Feature::CETakeFromHand, Attribute::CardAttributes) => true,
            (Feature::CESwapHand, Attribute::CardAttributes) => true,

            (Feature::CardConstraints, Attribute::CardAttributes) => true,
            (Feature::CardSuits, Attribute::CardAttributes) => true,
            (Feature::CardRanks, Attribute::CardAttributes) => true,
            (Feature::IgnoreConstraints, Attribute::CardAttributes) => true,
            (Feature::CardCompare, Attribute::CardAttributes) => true,
            (Feature::CardValues, Attribute::CardAttributes) => true,
            (Feature::ExceptionConstraints, Attribute::CardAttributes) => true,

            (Feature::PlayerHand, Attribute::PlayerAttributes) => true,
            (Feature::PlayerMoves, Attribute::PlayerAttributes) => true,

            (Feature::AnyTime, Attribute::Actions) => true,
            (Feature::StartTime, Attribute::Actions) => true,
            (Feature::TurnStartTime, Attribute::Actions) => true,
            (Feature::TurnEndTime, Attribute::Actions) => true,

            (Feature::WinCon, Attribute::GameAttributes) => true,
            (Feature::EndCon, Attribute::GameAttributes) => true,
            (Feature::TurnOrder, Attribute::GameAttributes) => true,

            (_, _) => false
        }
    }

    pub fn get_attribute(&self) -> Attribute {
        match self {
            Feature::CEChangeCard => Attribute::CardAttributes,
            Feature::CEDrawCard => Attribute::CardAttributes,
            Feature::CEGiveCard => Attribute::CardAttributes,
            Feature::CEPassNext => Attribute::CardAttributes,
            Feature::CETakeFromHand => Attribute::CardAttributes,
            Feature::CESwapHand => Attribute::CardAttributes,
            Feature::CardConstraints => Attribute::CardAttributes,
            Feature::CardSuits => Attribute::CardAttributes,
            Feature::CardRanks => Attribute::CardAttributes,
            Feature::IgnoreConstraints => Attribute::CardAttributes,
            Feature::CardCompare => Attribute::CardAttributes,
            Feature::CardValues => Attribute::CardAttributes,
            Feature::ExceptionConstraints => Attribute::CardAttributes,
            Feature::PlayerHand => Attribute::PlayerAttributes,
            Feature::PlayerMoves => Attribute::PlayerAttributes,
            Feature::AnyTime => Attribute::Actions,
            Feature::StartTime => Attribute::Actions,
            Feature::TurnStartTime => Attribute::Actions,
            Feature::TurnEndTime => Attribute::Actions,
            Feature::WinCon => Attribute::GameAttributes,
            Feature::EndCon => Attribute::GameAttributes,
            Feature::TurnOrder => Attribute::GameAttributes,
            _ => Attribute::None
        }
    }
}


impl Attribute {
    pub fn from_string(x: &str) -> Option<Attribute> {
        match x {
            "GAME" => Some(Attribute::GameAttributes),
            "CARD" => Some(Attribute::CardAttributes),
            "PLAYER" => Some(Attribute::PlayerAttributes),
            "ACTIONS" => Some(Attribute::Actions),
            _ => None
        }
    }
}