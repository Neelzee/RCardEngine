use crate::CDSL::{expr::{ParseError, ParseErrorCode}, parser::{get_card_fields, get_card_comperator}};

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

    pub fn validate_feature(x: String) -> Result<Feature, ParseError> {
        match x.split_whitespace().collect::<Vec<&str>>()[..] {
            ["change_card", ..] => match get_card_fields(x.split_whitespace().collect::<Vec<&str>>()[1..].to_vec()) {
                Ok(_) => Ok(Feature::CEChangeCard),
                Err(mut e) => match e.pop() {
                    Some(err) => Err(err),
                    None => Ok(Feature::CEChangeCard),
                },
            }

            ["swap_hand"] => Ok(Feature::CESwapHand),

            ["take_from_hand"] => Ok(Feature::CETakeFromHand),

            ["give_card"] => Ok(Feature::CEGiveCard),

            ["pass_next"] => Ok(Feature::CEPassNext),

            ["draw_card, .."] => match x.split_whitespace().collect::<Vec<&str>>()[1..].join(" ").parse::<i32>() {
                Ok(_) => Ok(Feature::CEDrawCard),
                Err(_) => Err(
                    ParseError {
                        err: ParseErrorCode::InvalidFeatureArgumentError,
                        expr: None,
                        str: x,
                    }
                ),
            }

            ["card_suits"] => Ok(Feature::CardSuits),

            ["card_ranks"] => Ok(Feature::CardRanks),

            ["card_values"] => Ok(Feature::CardValues),

            ["card_constraints"] => Ok(Feature::CardConstraints),

            ["ignore_constraints"] => Ok(Feature::IgnoreConstraints),

            ["player_hand"] => Ok(Feature::PlayerHand),

            ["player_moves"] => Ok(Feature::PlayerMoves),

            ["any_time"] => Ok(Feature::AnyTime),

            ["start_time"] => Ok(Feature::StartTime),

            ["exception_constraints", ..] => match get_card_comperator(&x.split_whitespace().collect::<Vec<&str>>()[1..].join("")) {
                Some(_) => Ok(Feature::ExceptionConstraints),
                None => Err(
                    ParseError {
                        err: ParseErrorCode::InvalidFeatureArgumentError,
                        expr: None,
                        str: x,
                    }
                ),
            }

            _ => Err(
                ParseError {
                    err: ParseErrorCode::NotAFeatureError,
                    expr: None,
                    str: x,
                }
            ),
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