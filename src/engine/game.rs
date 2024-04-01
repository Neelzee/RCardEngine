use serde::{Deserialize, Serialize};

use super::{
    card::Card,
    plr::{Move, Player},
};

use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize)]
pub struct Game {
    // Players playing the game
    players: Vec<Player>,
    moves: Vec<Move>,
    deck: Vec<(String, Vec<Card>)>,
    hand: u32,
    cards: Vec<Card>,
    turn_pointer: u32,
    actions: [Vec<String>; 8],
}

#[derive(Debug, Serialize, Deserialize)]
pub enum GameState {
    Start,
    PostStart,
    PreTurn,
    PostTurn,
    PreRound,
    PostRound,
    PostWin,
    PostEnd,
}
