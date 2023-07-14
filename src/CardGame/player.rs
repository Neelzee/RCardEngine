use super::card::Card;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Player {
    name: String,
    hand: Vec<Card>,
    moves: Vec<(Move, bool)>,
    move_history: Vec<(Move, bool)>,
    score: i32
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Move {
    PlayCard,
    DrawCard,
    Pass,
    DiscardCard
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

    pub fn set_moves(&mut self, moves: Vec<(Move, bool)>) {
        self.moves = moves;
    }

    pub fn add_to_hand(&mut self, mut hand: Vec<Card>) {
        self.hand.append(&mut hand);
    }

    pub fn get_move_history(&self) -> Vec<(Move, bool)> {
        self.move_history.clone()
    }
}


impl Move {
    pub fn from_string(xs: &str) -> Option<(Move, bool)> {
        let parts: Vec<&str> = xs.split_whitespace().collect();

        match &parts[..] {
            ["PLAYCARD", b] => Some((Move::PlayCard, b.to_owned() == "TRUE")),
            ["DRAWcARD", b] => Some((Move::DrawCard, b.to_owned() == "TRUE")),
            ["PASS", b] => Some((Move::Pass, b.to_owned() == "TRUE")),
            ["DISCARDCARD", b] => Some((Move::DiscardCard, b.to_owned() == "TRUE")),
            _ => None
        }
    }
}