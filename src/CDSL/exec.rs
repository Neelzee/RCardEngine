use crate::CardGame::game::Game;
use crate::CardGame::card::Card;
use super::expr::Expr;


pub fn place_card_stmt(xs: Vec<Expr>) -> impl Fn(Game, Card) -> bool {

    if xs.is_empty() || xs.get(0).unwrap() == Expr::Null {
        return |_, _| true;
    }

    if xs.into_iter().any(|x| x == Expr::CardRank || x == Expr::CardSuit || x == Expr::CardValue) {
        move |g, c| compare_cards(xs, g, c)
    }

    return |_, _| false;
}

pub fn compare_cards(xs: Vec<Card>, g: Game, c: Card) -> bool {
    false
}