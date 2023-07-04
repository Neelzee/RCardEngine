use super::Card::Card;

pub enum Move {
    Play,
    Draw,
    Pass,
    Discard
}

pub struct Player {
    name: String,
    hand: Vec<Card>,
    moves: Vec<(Move, bool)>,
    move_history: Vec<(Move, bool)>,
    score: i32
}


impl Player {
    pub fn new(plr: String) -> Player {
        Player {
            name: plr,
            hand: Vec::new(),
            moves: Vec::new(),
            move_history: Vec::new(),
            score: 0
        }
    }

}
