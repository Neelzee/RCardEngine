use crate::CardGame::player::Move;
use crate::CardGame::card::Card;
use crate::feature::Feature;

pub enum CardEffect {
    ChangeCard,
    SwapHand,
    TakeFromHand,
    GiveCard,
    PassNext,
    DrawCards,
    Blank
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    Any(Box<Expr>),
    All(Box<Expr>),
    Greatest(Box<Expr>),
    Players(Box<Expr>),
    Score,
    Hand,
    IsEqual(Box<Expr>, Box<Expr>),
    Numeric(i32),
    IsEmpty(Box<Expr>),
    If(Vec<Expr>, Vec<Expr>),
    Swap(Box<Expr>, Box<Expr>),
    Shuffle(Box<Expr>),
    Deck,
    Pile,
    Discard,
    Take(Box<Expr>, Box<Expr>, Box<Expr>),
    Always,
    Never,
    Not(Vec<Expr>),
    And(Vec<Expr>, Vec<Expr>),
    Or(Vec<Expr>, Vec<Expr>),
    AffectPlayer(String, Option<Vec<Expr>>),
    TOLeft,
    TORight,
    CardRank,
    CardSuit,
    CardValue,
    PlayerAction(Move, bool),
    CurrentPlayer(Box<Expr>),
    PreviousPlayer(Box<Expr>),
    Reset(Box<Expr>),
    Cards(Vec<Card>),
    Turn,   
    GoBack(Box<Expr>),
    GoForward(Box<Expr>),
    IsMove(Box<Expr>),
    Move(Box<Expr>),
    IsSame(Box<Expr>, Box<Expr>),
    Look(Box<Expr>, Box<Expr>),
    Put(Box<Expr>, Box<Expr>),
    Text(String),
    PMove,
    CLe,
    CGr,
    CEq,
    CLEq,
    CGRq,
    PAPass,
    PADraw,
    PAPlay,
    Null
}

impl Expr {
    pub fn is_numeric(&self) -> bool {
        match self {
            Expr::Numeric(_) => true,
            Expr::Score => true,
            _ => false
        }
    }

    pub fn to_numeric(&self) -> Option<Expr> {
        match self {
            Expr::Text(t) => match t.parse::<i32>() {
                Ok(i) => Some(Expr::Numeric(i)),
                Err(_) => None,
            }
            
            Expr::Numeric(_) => Some(self.clone()),
            
            _ => None
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Expr::Any(_) => todo!(),
            Expr::All(_) => todo!(),
            Expr::Greatest(_) => todo!(),
            Expr::Players(_) => todo!(),
            Expr::Score => todo!(),
            Expr::Hand => todo!(),
            Expr::IsEqual(_, _) => todo!(),
            Expr::Numeric(_) => todo!(),
            Expr::IsEmpty(_) => todo!(),
            Expr::If(_, _) => todo!(),
            Expr::Swap(_, _) => todo!(),
            Expr::Shuffle(_) => todo!(),
            Expr::Deck => todo!(),
            Expr::Pile => todo!(),
            Expr::Discard => todo!(),
            Expr::Take(_, _, _) => todo!(),
            Expr::Always => todo!(),
            Expr::Never => todo!(),
            Expr::Not(_) => todo!(),
            Expr::And(_, _) => todo!(),
            Expr::Or(_, _) => todo!(),
            Expr::AffectPlayer(_, _) => todo!(),
            Expr::TOLeft => todo!(),
            Expr::TORight => todo!(),
            Expr::CardRank => todo!(),
            Expr::CardSuit => todo!(),
            Expr::CardValue => todo!(),
            Expr::PlayerAction(_, _) => todo!(),
            Expr::CurrentPlayer(_) => todo!(),
            Expr::PreviousPlayer(_) => todo!(),
            Expr::Reset(_) => todo!(),
            Expr::Cards(_) => todo!(),
            Expr::Turn => todo!(),
            Expr::GoBack(_) => todo!(),
            Expr::GoForward(_) => todo!(),
            Expr::IsMove(_) => todo!(),
            Expr::Move(_) => todo!(),
            Expr::IsSame(_, _) => todo!(),
            Expr::Look(_, _) => todo!(),
            Expr::Put(_, _) => todo!(),
            Expr::Text(_) => todo!(),
            Expr::CLe => todo!(),
            Expr::CGr => todo!(),
            Expr::CEq => todo!(),
            Expr::CLEq => todo!(),
            Expr::CGRq => todo!(),
            Expr::Null => todo!(),
            Expr::PMove => todo!(),
            Expr::PAPass => todo!(),
            Expr::PADraw => todo!(),
            Expr::PAPlay => todo!(),
        }
    }
}


#[derive(Clone)]
pub struct ParseError {
    err: ParseErrorCode,
    expr: Option<Expr>,
    str: String
}

impl ParseError {
    pub fn new(err: ParseErrorCode, expr: Option<Expr>, str: String) -> ParseError {
        ParseError {
            err,
            expr,
            str,
        }
    }

    pub fn get_error(&self) -> ParseErrorCode {
        self.err.clone()
    }

    pub fn get_expr(&self) -> Option<Expr> {
        self.expr.clone()
    }

    pub fn get_string(&self) -> String {
        self.str.clone()
    }
}

#[derive(Clone)]
pub enum ParseErrorCode {
    IncompleteExpressionError,
    SyntaxError,
    ValidateFeatureError(Feature, Vec<ParseError>),
    ValidateExpressionError(Option<Feature>, Vec<ParseError>),
    NotAFeatureError,
    NotAnAttributeError,
    NotAFeatureOfAttributeError,
    NotANumberError,
    InvalidFeatureArgumentError,
    NotACardFieldError,
    MissMatchCardError(Vec<Card>, Vec<Card>),
    InvalidExpressionError,
    InvalidFilePath
}


pub struct ExecError {
    err: ExecErrorCode,
    expr: Expr
}

pub enum ExecErrorCode {
    InvalidSyntaxError,
    SyntaxErrorRightOperand,
    SyntaxErrorLeftOperand,
    InvalidBoolEvaluationError,
    UnknownExpressionError
}