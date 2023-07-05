use super::expr::Expr;
use super::expr::ParseError;
use super::expr::ParseErrorCode;
use crate::CardGame::card::Card;
use crate::CardGame::player::Move;
use crate::feature::Feature;
use crate::functions::string_to_list;

pub fn parse_cdsl_player_action(xs: &str) -> Result<Vec<Expr>, ParseError> {
    let mut exprs = Vec::new();

    for e in parse_string_to_list(xs) {
        match e {
            Expr::Text(t) => {
                match t.find(" ") {
                    Some(index) => {
                        let (w, b) = t.split_at(index);
                        match w {
                            "PLAYCARD" => exprs.push(Expr::PlayerAction(Move::PlayCard, b == "TRUE")),

                            "DRAWCARD" => exprs.push(Expr::PlayerAction(Move::DrawCard, b == "TRUE")),

                            "DISCARDCARD" => exprs.push(Expr::PlayerAction(Move::DiscardCard, b == "TRUE")),

                            "PASS" => exprs.push(Expr::PlayerAction(Move::Pass, b == "TRUE")),

                            _ => return Err(ParseError {
                                err: ParseErrorCode::SyntaxError,
                                expr: Some(e),
                                str: w.to_owned()})
                        }
                    }

                    None => return Err(ParseError {
                        err: ParseErrorCode::SyntaxError,
                        expr: Some(e),
                        str: t})
                }
            }

            _ => {
                return Err(ParseError {
                    err: ParseErrorCode::SyntaxError,
                    expr: Some(e),
                    str: e.to_string()
                })
            }
        }
    }

    Ok(exprs)
}

pub fn parse_string_to_list(xs: &str) -> Vec<Expr> {
    let exprs = xs.split(",")
        .into_iter()
        .map(|x| Expr::Text(x.trim().to_owned())).collect::<Vec<Expr>>();

    return exprs;
}


pub fn process_if_string(xs: &str) -> (Vec<Vec<&str>>, Vec<Vec<&str>>) {

    let mut left: Vec<Vec<&str>> = Vec::new();
    let mut right: Vec<Vec<&str>> = Vec::new();

    match xs.find(":") {
        Some(index) => {
            let (l, r) = xs.split_at(index);

            for x in l.split(",") {
                left.push(x.split_whitespace().collect::<Vec<&str>>());
            }

            for x in r.split(",") {
                right.push(x.split_whitespace().collect::<Vec<&str>>());
            }

            return (left, right);
        }
        None => (left, right)
    }
}


pub fn get_card_fields(xs: Vec<&str>) -> Result<Vec<Expr>, Vec<ParseError>> {
    let mut exprs = Vec::new();
    let mut errs = Vec::new();

    for x in xs {
        match x.trim() {
            "ranks" => exprs.push(Expr::CardRank),
            "suits" => exprs.push(Expr::CardSuit),
            "values" => exprs.push(Expr::CardValue),
            _ => errs.push(ParseError { err: ParseErrorCode::NotACardFieldError, expr: Some(Expr::Null), str: x.to_owned() })
        }
    }

    return if errs.is_empty() { Ok(exprs) } else { Err(errs) }
}

pub fn read_cdsl(xs: &str) -> Result<((Feature, Option<Vec<Expr>>), Vec<Expr>), (Option<Feature>, Vec<ParseError>)> {
    todo!()
}

pub fn get_cards(xs: Vec<&str>) -> Vec<Card> {
    xs.into_iter()
        .filter_map(|x| if x.contains(".") { Some(x) } else { None } )
        .into_iter()
        .map(|x| Card::new(x.split(".").collect::<Vec<&str>>()[0].to_owned(), x.split(".").collect::<Vec<&str>>()[1].to_owned(), 0))
        .into_iter()
        .collect::<Vec<Card>>()
}


pub fn parse_expr(mut xs: Vec<&str>) -> Result<Vec<Expr>, Vec<ParseError>> {
    let mut exprs = Vec::new();

    let mut errs = Vec::new();


    let mut expr = Expr::Null;

    for w in xs {
        // TODO: Add check for not

        match w {
            "||" => todo!(),
            "&&" => todo!(),
            ":" => todo!(),
            _ => todo!()
        }
    }

    return if errs.is_empty() { Ok(exprs) } else { Err(errs) }
}


pub fn parse_if_expr(xs: &str) -> Result<Expr, Vec<ParseError>> {
    match xs.find(":") {
        Some(index) => {
            let (lr, rr) = xs.split_at(index);
            match (parse_expr(string_to_list(lr)), parse_expr(string_to_list(rr))) {
                (Ok(l), Ok(r)) => Ok(Expr::If(l, r)),
                (Ok(_), Err(r)) => Err(r),
                (Err(l), Ok(_)) => Err(l),
                (Err(mut l), Err(mut r)) => {
                    l.append(&mut r);
                    Err(l)
                }
            }
        }
        None => Err(ParseError { err: SyntaxError, expr: Some(Expr::If(Expr::Null, Expr::Null)), str: xs.to_owned() }),
    }
}

pub fn parse_one_cdsl(mut xs: Vec<&str>) -> Result<(Expr, Vec<&str>), (ParseError, i32)> {
    let mut i = 0;
    
    loop {
        match xs.pop() {
            Some(x) => {
                i += 1;
                match x {
                    "any" => todo!(),
                    "all" => todo!(),
                    "greatest" => todo!(),
                    "players" => todo!(),
                    "score" => todo!(),
                    "hand" => todo!(),
                    "isEqual" => todo!(),
                    "isEmpy" => todo!(),
                    "swap" => todo!(),
                    "shuffle" => todo!(),
                    "prevPlayer" => todo!(),
                    "deck" => todo!(),
                    "pile" => todo!(),
                    "take" => todo!(),
                    "always" => todo!(),
                    "never" => todo!(),
                    "ranks" => todo!(),
                    "suits" => todo!(),
                    "values" => todo!(),
                    "discard" => todo!(),
                    "left" => todo!(),
                    "right" => todo!(),
                    "turn" => todo!(),
                    "goBack" => todo!(),
                    "goForward" => todo!(),
                    "reset" => todo!(),
                    "player" => todo!(),
                    "moves" => todo!(),
                    "le" => todo!(),
                    "ge" => todo!(),
                    "lte" => todo!(),
                    "gte" => todo!(),
                    "eq" => todo!(),
                    "isMove" => todo!(),
                    "pPass" => todo!(),
                    "pDraw" => todo!(),
                    "pPlay" => todo!(),
                    "isSame" => todo!(),
                    "look" => todo!(),
                    "put" => todo!(),
                    "null" => todo!(),
                    _ => match x.parse::<i32>() {
                        Ok(i) => Ok((Expr::Numeric(i), xs)),
                        Err(_) => Ok((Expr::Text(x.to_owned()), xs)),
                    }
                }
            }
            None => Err((ParseError { err: todo!(), expr: todo!(), str: todo!() }, i)),
        }
    }
}