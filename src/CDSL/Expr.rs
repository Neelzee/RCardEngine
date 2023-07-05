use crate::CardGame::Player::Move;
use crate::CardGame::Card::Card;

#[derive(Clone, Copy, Debug)]
pub enum CardEffect {
    ChangeCard,
    SwapHand,
    TakeFromHand,
    GiveCard,
    PassNext,
    DrawCards,
    Blank
}

#[derive(Clone, Debug)]
pub enum Expr {
    // ATM, only used to signal Any Player
    Any(Box<Expr>),
    // Only used to signal All Players
    All(Box<Expr>),
    // Should return the greatest of a list
    Greatest(Box<Expr>),
    // List of all Players
    Players(Box<Expr>),
    // Score of a Player
    Score,
    // The hand of a player
    Hand,
    // Evals to true if two expressions are equal
    IsEqual(Box<Expr>, Box<Expr>),
    // Numeric expression
    Numeric(i32),
    // Evals to true if a given list is empty
    IsEmpty(Box<Expr>),
    // If, a list of expression all evals too true, then execute a list of expressions
    If(Vec<Expr>, Vec<Expr>),
    // Swap the data in two different lists
    Swap(Box<Expr>, Box<Expr>),
    // Shuffle a list
    Shuffle(Box<Expr>),
    // References the Game Deck
    Deck,
    // References the Game Pile
    Pile,
    // Discard pile
    Discard,
    // Takes a given number from one list, and appends it to the other
    Take(Box<Expr>, Box<Expr>, Box<Expr>),
    // Evals to true
    Always,
    // Evals to false
    Never,
    // Negates an boolean
    Not(Vec<Expr>),
    // Evals too true if both conditions are true
    And(Vec<Expr>, Vec<Expr>),
    // Evals too true if any condition is true
    Or(Vec<Expr>, Vec<Expr>),
    // The following expression affects the next player
    AffectPlayer(CardEffect, Option<Vec<Expr>>),
    // References the turn order
    TOLeft,
    TORight,
    // References Card Rank
    CardRank,
    // References Card Suit
    CardSuit,
    // References Card Value
    CardValue,
    // References Player Action
    PlayerAction(Move, bool),
    // Player Action Pass
    PAPass,
    // Player Action Draw
    PADraw,
    // Player Action Play
    PAPlay,
    // CardEffect
    CEffect(CardEffect, Vec<Card>),
    // Current Player
    CurrentPlayer(Box<Expr>),
    // Previous Player
    PreviousPlayer(Box<Expr>),
    // Resets a player ability
    Reset(Box<Expr>),
    // List of cards
    Cards(Vec<Card>),
    // References the turnsystem
    Turn,
    // Makes the turn go back :)
    GoBack(Box<Expr>),
    GoForward(Box<Expr>),
    // Evals too true, if the Current Players last move was the given move
    IsMove(Box<Expr>),
    // Moves to the player
    PMoves,
    // checks if the given list has the is same
    IsSame(Box<Expr>, Box<Expr>),
    // Takes the given amount of elements out of alist
    Look(Box<Expr>, Box<Expr>),
    // Puts the elements in the list,(i32),o the other list
    Put(Box<Expr>, Box<Expr>),
    // A string
    Text(String),
    // Lesser
    CLe,
    // Greater
    CGr,
    // Equal
    CEq,
    // Lesser Or Equal
    CLEq,
    // Greater or Equal
    CGRq,
    // Null value, is not used in normal expressions, and just in error messages
    Null
}