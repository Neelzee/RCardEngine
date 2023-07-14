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

#[derive(Clone, Debug)]
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
    can_place_card: Vec<F>,
    turn_order: Expr,
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
            can_place_card: Vec::new(),
            turn_order: Expr::TOLeft,
        }
    }

    pub fn get_deck(&self) -> Vec<Card> {
        self.deck.clone()
    }

    pub fn get_pile(&self) -> Vec<(Card, Option<Card>)> {
        self.pile.clone()
    }

    pub fn get_discard(&self) -> Vec<Card> {
        self.discard.clone()
    }

    pub fn set_deck(&mut self, deck: Vec<Card>) {
        self.deck = deck;
    }

    pub fn set_pile(&mut self, pile: Vec<Card>) {
        self.pile = pile.into_iter().map(|c| (c, None)).collect::<Vec<(Card, Option<Card>)>>();
    }

    pub fn update_pile(&mut self, pile: Vec<(Card, Option<Card>)>) {
        self.pile = pile;
    }

    pub fn set_discard(&mut self, discard: Vec<Card>) {
        self.discard = discard;
    }

    pub fn get_players(&self) -> CircularList<Player> {
        self.players.clone()
    }

    pub fn set_players(&mut self, players: CircularList<Player>) {
        self.players = players;
    }

    pub fn update_player(&mut self, player: Player) {
        self.players.update(player);
    }

    pub fn get_player_moves(&self) -> Vec<(Move, bool)> {
        self.player_moves.clone()
    }

    pub fn set_turn_order(&mut self, expr: Expr) {
        self.turn_order = expr;
    }

    pub fn get_turn_order(&self) -> Expr {
        self.turn_order.clone()
    }

    pub fn next(&mut self) {
        match self.turn_order {
            Expr::TOLeft => {
                self.players.rot_l();
            }

            Expr::TORight => {
                self.players.rot_r();
            }

            _ => self.players.rot()
        }
    }

    pub fn prev(&mut self) {
        match self.turn_order {
            Expr::TOLeft => {
                self.players.rot_r();
            }

            Expr::TORight => {
                self.players.rot_l();
            }

            _ => {
                self.players.reverse_rotation();
                self.players.rot();
                self.players.reverse_rotation();
            }
        }
    }
}
