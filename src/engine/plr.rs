use super::card::Card;

pub struct Player {
    symbol: char,
    name: String,
    moves: Vec<Move>,
    hand: Vec<Card>,
}

pub enum Move {
    Play(bool),
    Draw(u32, bool),
    Pass,
}
