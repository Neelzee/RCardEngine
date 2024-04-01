mod engine;

use engine::load_game;

fn main() {
    match load_game() {
        Ok(_) => (),
        Err(err) => println!("Failed with err: {err:?}"),
    }
}
