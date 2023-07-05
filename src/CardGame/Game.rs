use super::Player::Player;
use super::Player::Move;
use super::Card::Card;
use crate::Feature::Feature;
use crate::CDSL::Expr::Expr;

#[derive(Clone, Copy, Debug)]
pub enum GameState {
    Start,
    TurnStart,
    TurnEnd,
    RoundStart,
    RoundEnd,
    End
}

pub struct Game {
    game_name: String,
    state: GameState,
    player_moves: Vec<(Move, bool)>,
    card_gen: Vec<Card>,
    discard: Vec<Card>,
    deck: Vec<Card>,
    pile: Vec<(Card, Option<Card>)>,
    players: Vec<Player>,
    rules: Vec<(Feature, Vec<Expr>)>,
    actions: Vec<(GameState, Vec<Expr>)>,
    can_place_card: Vec<String>
}


impl Game {
    pub fn new(name: String, plrs: Vec<String>) -> Game {
        Game {
            game_name: name,
            state: GameState::Start,
            players: Vec::new(),
            player_moves: Vec::new(),
            card_gen: Vec::new(),
            discard: Vec::new(),
            deck: Vec::new(),
            pile: Vec::new(),
            rules: Vec::new(),
            actions: Vec::new(),
            can_place_card: Vec::new()
        }
    }
}
