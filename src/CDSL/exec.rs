use crate::CardGame::Game::Game;

use super::Expr::Expr;


pub fn place_card_stmt(xs: Vec<Expr>) -> impl Fn(Game, Card) -> bool {

    if len(xs) == 0 || xs.0 == Expr::Null {
        return |_, _| true;
    }

    if any(xs, |x| x == Expr::CardRank || x == Expr::CardSuit || x == CardValue) {
        move |g, c| compare_cards(xs, g, c)
    }

    return |_, _| false;
}

pub fn compare_cards(xs: Vec<Card>, g: Game, c: Card) -> bool {
    false
}