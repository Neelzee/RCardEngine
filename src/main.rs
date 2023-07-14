use RCardEngine::CardGame::{game::Game, card::Card};


fn main() {
    println!("Hello, world!");
}



fn play_game(n: i32) {

}


fn get_games<F>() -> Option<Vec<Game<F>>>
where
    F: Fn(Game<F>, Card) -> bool
{
    None
}
