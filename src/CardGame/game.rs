use crate::CircularList::circular_list::CircularList;
use crate::feature::Feature;
use super::player::Player;
use super::player::Move;
use super::card::Card;
use crate::CDSL::expr::Expr;

#[derive(Clone, Copy, Debug)]
pub enum GameState {
    Start,
    TurnStart,
    TurnEnd,
    RoundStart,
    RoundEnd,
    End
}

pub struct Game<F> 
where
    F: Fn(Game<F>, Card) -> bool
{
    game_name: String,
    state: GameState,
    player_moves: Vec<(Move, bool)>,
    card_gen: Vec<Card>,
    discard: Vec<Card>,
    deck: Vec<Card>,
    pile: Vec<(Card, Option<Card>)>,
    players: CircularList<Player>,
    rules: Vec<(Feature, Vec<Expr>)>,
    actions: Vec<(GameState, Vec<Expr>)>,
    can_place_card: Vec<F>
}


impl<F: Fn(Game<F>, Card) -> bool> Game<F> {
    pub fn new(game_name: String, players: Vec<Player>) -> Game<F> {
        Game {
            game_name,
            state: GameState::Start,
            players: CircularList::from_list(players),
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
