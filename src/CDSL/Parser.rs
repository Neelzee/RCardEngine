use super::Expr::Expr;
use super::Expr::CardEffect;
use crate::CardGame::Card::Card;
use crate::CardGame::Player::Move;
use crate::Feature::Feature;
use crate::functions;

impl Expr {
    pub fn is_expr_numeric(&self) -> bool {
        match self {
            Expr::Numeric(_) => true,
            Expr::Score => true,
            _ => false
        }
    }

    pub fn to_numeric(&self) -> Option<Expr> {
        match self {
            Expr::Text(t) => {
                match t.parse::<i32>() {
                    Ok(i) => {
                        return Some(Expr::Numeric(i));
                    },
                    Err(_) => {
                        return None;
                    }
                }
            }

            Expr::Numeric(i) => {
                return Some(Expr::Numeric(*i));
            }

            _ => {
                return None;
            }
        }
    }
}

pub fn parse_string_list(xs: &str) -> Vec<Expr> {

    let mut lst: Vec<Expr> = Vec::new();

    for wrd in xs.split_whitespace() {
        lst.push(Expr::Text(String::from(wrd)));
    }

    return lst;
}

pub fn parse_expr_player_action(xs: &str) -> Option<Vec<Expr>> {
    let mut exprs: Vec<Expr> = Vec::new();

    let wrds = parse_string_list(xs);

    for wrd in wrds {
        match wrd {
            Expr::Text(w) => {
                let parts: Vec<&str> = w.split(",").collect();
                match &parts[..] {
                    ["PLAYCARD", b] => {
                        exprs.push(Expr::PlayerAction(Move::PlayCard, b.to_owned() == "TRUE"));
                    }
                    ["DRAWCARD", b] => {
                        exprs.push(Expr::PlayerAction(Move::DrawCard, b.to_owned() == "TRUE"));
                    }
                    ["PASS", b] => {
                        exprs.push(Expr::PlayerAction(Move::Pass, b.to_owned() == "TRUE"));
                    }
                    ["DISCARDCARD", b] => {
                        exprs.push(Expr::PlayerAction(Move::DiscardCard, b.to_owned() == "TRUE"));
                    }
                    _ => {
                        return None;
                    }
                }
            }
            _ => {
                return None;
            }
        }
    }

    Some(exprs)
}

pub fn process_if_string(x: &str) -> Option<(Vec<Vec<&str>>, Vec<Vec<&str>>)> {
    let mut left: Vec<Vec<&str>> = Vec::new();
    let mut right: Vec<Vec<&str>> = Vec::new();
    
    match x.find(":") {
        Some(index) => {
            let (raw_l, raw_r) = x.split_at(index);

            left.push(raw_l.split_whitespace().collect());
            right.push(raw_r.split_whitespace().collect());
        },
        None => {
            return None;
        }
    }

    

    return Some((left, right));
}


pub fn get_card_fields(xs: String) -> Option<Vec<Expr>> {
    let mut fields: Vec<Expr> = Vec::new();

    for wrd in xs.split_whitespace() {
        match wrd {
            "ranks" => {
                fields.push(Expr::CardRank);
            }
            "suits" => {
                fields.push(Expr::CardSuit);
            }
            "values" => {
                fields.push(Expr::CardValue);
            }
            _ => {
                return None;
            }
        }
    }

    return Some(fields);
}

pub fn read_cdsl(xs: String) -> Option<((Feature, Option<Vec<Expr>>), Vec<Expr>)> {

    match xs.find("=") {
        Some(index) => {
            let (y, ys) = xs.split_at(index);
            
            match validate_feature(y.to_owned()) {

                Some(Feature::CEDrawCard) => {
                    match y.parse::<i32>() {
                        Ok(i) => {
                            return Some(((Feature::CEDrawCard, Some([Expr::Numeric(i)].to_vec())), [Expr::CEffect(CardEffect::DrawCards, get_cards(functions::string_to_list(ys)))].to_vec()));
                        }
                        Err(_) => {
                            return None;
                        }
                    }
                }

                Some(Feature::CESwapHand) => {
                    return Some(((Feature::CESwapHand, None), [Expr::CEffect(CardEffect::DrawCards, get_cards(functions::string_to_list(ys)))].to_vec()));
                }

                Some(Feature::CEChangeCard) => {
                    let parts: Vec<&str> = y.split_whitespace().collect();
                    let wrds = parts[1..].join(" ");
                    match get_card_fields(wrds) {
                        Some(res) => {
                            return Some(
                                (    
                                    (Feature::CEChangeCard, Some(res)),
                                    [Expr::CEffect(CardEffect::ChangeCard, get_cards(functions::string_to_list(ys)))].to_vec()
                                )
                            );
                        },
                        None => todo!(),
                    }
                }

                Some(Feature::CETakeFromHand) => {
                    return Some((
                        (Feature::CETakeFromHand, None),
                        [Expr::CEffect(CardEffect::TakeFromHand, get_cards(functions::string_to_list(ys)))].to_vec()
                    ));
                }

                Some(Feature::CEPassNext) => {
                    return Some((
                        (Feature::CEPassNext, None),
                        [Expr::CEffect(CardEffect::PassNext, get_cards(functions::string_to_list(ys)))].to_vec()
                    ));
                }

                Some(Feature::CEGiveCard) => {
                    return Some((
                        (Feature::CEGiveCard, None),
                        [Expr::CEffect(CardEffect::GiveCard, get_cards(functions::string_to_list(ys)))].to_vec()
                    ));
                }

                Some(Feature::PlayerMoves) => {
                    return Some((
                        (Feature::PlayerMoves, None),
                        functions::string_to_list(ys)
                            .into_iter()
                            .filter_map(|x| Move::from_string(x)
                            .map(|(m, b)| Expr::PlayerAction(m, b)))
                            .collect(),
                    ));
                }

                Some(Feature::ExceptionConstraints) => todo!(),
                Some(Feature::IgnoreConstraints) => todo!(),
                Some(_) => todo!(),
                None => {
                    return None;
                },
            }
        }
        None => todo!(),
    }


    None
}

pub fn get_cards(xs: Vec<String>) -> Vec<Card> {

    let mut cards: Vec<Card> = Vec::new();

    for x in xs {
        let parts: Vec<&str> = x.split(".").collect();
        match &parts[..] {
            [s, r] => {
                cards.push(Card::new(String::from(s.to_owned()), String::from(r.to_owned()), 0));
            }
            [s] => {
                cards.push(Card::new(String::from(s.to_owned()), String::new(), 0));
            }
            _ => {
                continue;
            }
        }
    }

    return cards;
}

pub fn validate_feature(x: String) -> Option<Feature> {
    let parts: Vec<&str> = x.split_whitespace().collect();
    match parts[0] {
        "change_card" => {
            match get_card_fields(parts[1..].join("")) {
                Some(_) => Some(Feature::CEChangeCard),
                None => None
            }
        }
        "swap_hand" => Some(Feature::CESwapHand),
        "take_from_hand" => Some(Feature::CETakeFromHand),
        "give_card" => Some(Feature::CEGiveCard),
        "pass_next" => Some(Feature::CEPassNext),
        "draw_card" => {
            match parts[1..].join("").parse::<i32>() {
                Ok(i) => Some(Feature::CEDrawCard),
                Err(_) => None
            }
        }
        "card_suits" => Some(Feature::CardSuits),
        "card_values" => Some(Feature::CardValues),
        "card_ranks" => Some(Feature::CardRanks),
        "card_constraints" => Some(Feature::CardConstraints),
        "ignore_constraints" => Some(Feature::IgnoreConstraints),
        "player_hand" => Some(Feature::PlayerHand),
        "player_moves" => Some(Feature::PlayerMoves),
        "any_time" => Some(Feature::AnyTime),
        "start_time" => Some(Feature::StartTime),
        "exception_constraints" => {
            match get_card_comperator(&parts[1..].join("")) {
                Some(_) => Some(Feature::ExceptionConstraints),
                None => None,
            }
        }
        _ => Feature::from_string(&x)
    }
}


pub fn get_card_comperator(x: &str) -> Option<Expr> {
    match x {
        "eq" => Some(Expr::CEq),
        "lte" => Some(Expr::CLEq),
        "gte" => Some(Expr::CGRq),
        "le" => Some(Expr::CLe),
        "ge" => Some(Expr::CGr),
        _ => None
    }
}