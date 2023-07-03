pub struct Card {
    suit: String,
    rank: String,
    value: i32
}

impl Card {
    fn new(suit: String, rank: String, value: i32) -> Card {
        Card {
            suit: suit,
            rank: rank,
            value: value
        }
    }
}
