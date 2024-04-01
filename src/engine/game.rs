use super::{card::Card, plr::Player};

use std::collections::HashMap;

pub struct Game {
    // Players playing the game
    plrs: Vec<Player>,
    deck: Vec<Card>,
    // Discard pile
    disc: Vec<Card>,
    pile: Vec<Card>,
    end_con: Vec<Box<dyn Fn(Game) -> bool>>,
    win_con: Vec<Box<dyn Fn(Game) -> Player>>,
    // Actions that will be executed at specific game states
    acts: HashMap<GameState, Vec<Box<dyn Fn(Game) -> Game>>>,
}

pub enum GameState {}
