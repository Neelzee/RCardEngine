use serde::{Deserialize, Serialize};

use super::card::Card;

#[derive(Debug, Serialize, Deserialize)]
pub struct Player {
    symbol: char,
    name: String,
    moves: Vec<Move>,
    hand: Vec<Card>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Move {
    Play,
    Draw,
    Pass,
}
