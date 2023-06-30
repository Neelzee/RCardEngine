pub mod Game {

pub enum GameState {
    Start,
    TurnStart,
    TurnEnd,
    RoundStart,
    RoundEnd,
    End
}

pub struct Game {
    game_name: String,
    state: GameState,
    player_moves: Vec<(String, bool)>,
    card_gen: Vec<String>,
    discard: Vec<String>,
    deck: Vec<String>,
    pile: Vec<(String, Option<String>)>,
    players: Vec<String>,
    rules: Vec<(String, Vec<String>)>,
    actions: Vec<(GameState, Vec<String>)>,
    can_place_card: Vec<String>
}

}
