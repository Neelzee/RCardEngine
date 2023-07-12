#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Card {
    suit: String,
    rank: String,
    value: i32
}

impl Card {
    pub fn new(suit: &str, rank: &str, value: i32) -> Card {
        Card {
            suit: suit.to_owned(),
            rank: rank.to_owned(),
            value,
        }
    }
}
