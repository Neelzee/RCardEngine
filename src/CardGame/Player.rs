use super::Card::Card;

pub struct Player {
    name: String,
    hand: Vec<Card>,
    moves: Vec<(String, bool)>,
    move_history: Vec<(String, bool)>,
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
