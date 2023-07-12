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
        match e.clone() {
            Expr::Text(t) => {
                match t.find(" ") {
                    Some(index) => {
                        let (w, b) = t.split_at(index);
                        match w {
                            "PLAYCARD" => exprs.push(Expr::PlayerAction(Move::PlayCard, b == "TRUE")),

                            "DRAWCARD" => exprs.push(Expr::PlayerAction(Move::DrawCard, b == "TRUE")),

                            "DISCARDCARD" => exprs.push(Expr::PlayerAction(Move::DiscardCard, b == "TRUE")),

                            "PASS" => exprs.push(Expr::PlayerAction(Move::Pass, b == "TRUE")),

                            _ => return Err(ParseError::new(ParseErrorCode::SyntaxError, Some(e), w.to_owned()))
                        }
                    }

                    None => return Err(ParseError::new(
                        ParseErrorCode::SyntaxError,
                        Some(e),
                        t))
                }
            }

            _ => {
                return Err(ParseError::new(
                    ParseErrorCode::SyntaxError,
                    Some(e.clone()),
                    e.to_string()
                ))
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
            _ => errs.push(ParseError::new(ParseErrorCode::NotACardFieldError, Some(Expr::Null), x.to_owned()))
        }
    }

    return if errs.is_empty() { Ok(exprs) } else { Err(errs) }
}

pub fn get_cards(xs: Vec<&str>) -> Vec<Card> {
    xs.into_iter()
        .filter_map(|x| if x.contains(".") { Some(x) } else { None } )
        .into_iter()
        .map(|x| Card::new(x.split(".").collect::<Vec<&str>>()[0], x.split(".").collect::<Vec<&str>>()[1], 0))
        .into_iter()
        .collect::<Vec<Card>>()
}

/**
 * Takes in a list of lines, then 
 */
pub fn parse_expr(mut xs: Vec<&str>) -> Result<Vec<Expr>, Vec<ParseError>> {
    todo!()
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
        None => Err(vec![ParseError::new(ParseErrorCode::SyntaxError, Some(Expr::If(vec![Expr::Null], vec![Expr::Null])), xs.to_owned())]),
    }
}

pub fn parse_one_cdsl(mut xs: Vec<&str>, mut i: i32) -> Result<(Expr, Vec<&str>), (ParseError, i32)> {
    
    todo!()
    /*
    match xs.pop() {
        Some(x) => {
            i += 1;
            match x {
                "any" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::Any(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::Any(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "all" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::All(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::All(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "greatest" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::Greatest(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::Greatest(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "players" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::Players(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::Players(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "score" => Ok((Expr::Score, xs)),

                "hand" => Ok((Expr::Hand, xs)),

                "isEqual" => match parse_one_cdsl(xs, i) {
                    Ok((l, ys)) => {
                        match parse_one_cdsl(ys, i - (xs.len() as i32) - (ys.len() as i32)) {
                            Ok((r, zs)) => {
                                Ok((Expr::IsEqual(Box::new(l), Box::new(r)), zs))
                            }

                            Err((e, j)) => {
                                let b = match e.get_expr() {
                                    Some(e) => Box::new(e),
                                    None => Box::new(Expr::Null),
                                };
        
                                let expr = Some(Expr::IsEqual(Box::new(l), b));
        
                                Err((
                                    ParseError {
                                        err: e.get_error(),
                                        expr,
                                        str: x.to_owned() + " " + &e.get_string(),
                                    },
                                    j
                                ))
                            }
                        }
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::IsEqual(b, Box::new(Expr::Null)));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "isEmpy" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::IsEmpty(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::IsEmpty(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "swap" => match parse_one_cdsl(xs, i) {
                    Ok((l, ys)) => {
                        match parse_one_cdsl(ys, i - (xs.len() as i32) - (ys.len() as i32)) {
                            Ok((r, zs)) => {
                                Ok((Expr::Swap(Box::new(l), Box::new(r)), zs))
                            }

                            Err((e, j)) => {
                                let b = match e.get_expr() {
                                    Some(e) => Box::new(e),
                                    None => Box::new(Expr::Null),
                                };
        
                                let expr = Some(Expr::Swap(Box::new(l), b));
        
                                Err((
                                    ParseError {
                                        err: e.get_error(),
                                        expr,
                                        str: x.to_owned() + " " + &e.get_string(),
                                    },
                                    j
                                ))
                            }
                        }
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::Swap(b, Box::new(Expr::Null)));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }
                
                "shuffle" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::Shuffle(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::Shuffle(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "prevPlayer" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::PreviousPlayer(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::PreviousPlayer(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "deck" => Ok((Expr::Deck, xs)),

                "pile" => Ok((Expr::Pile, xs)),

                "take" => match parse_one_cdsl(xs, i) {
                    Ok((Expr::Numeric(n), ys)) => match parse_one_cdsl(ys, i - ((xs.len() as i32) - (ys.len() as i32))) {
                        Ok((l, zs)) => match parse_one_cdsl(zs, i - ((xs.len() as i32) - (zs.len() as i32))) {
                            Ok((r, ws)) => Ok((Expr::Take(Box::new(Expr::Numeric(n)), Box::new(l), Box::new(r)), ws)),

                            Err((e, j)) => {

                                let r = match e.get_expr() {
                                    Some(e) => Box::new(e),
                                    None => Box::new(Expr::Null),
                                };
        
                                let expr = Some(Expr::Take(Box::new(Expr::Numeric(i)), Box::new(l), r));
        
                                Err((
                                    ParseError {
                                        err: e.get_error(),
                                        expr,
                                        str: x.to_owned() + " " + &e.get_string(),
                                    },
                                    j
                                ))
                            }
                        }

                        Err((e, j)) => {

                            let l = match e.get_expr() {
                                Some(e) => Box::new(e),
                                None => Box::new(Expr::Null),
                            };
    
                            let expr = Some(Expr::Take(Box::new(Expr::Numeric(i)), l, Box::new(Expr::Null)));
    
                            Err((
                                ParseError {
                                    err: e.get_error(),
                                    expr,
                                    str: x.to_owned() + " " + &e.get_string(),
                                },
                                j
                            ))
                        }
                    }

                    Ok((e, _)) => Err((
                        ParseError {
                            err: ParseErrorCode::NotANumberError,
                            expr: Some(Expr::Take(Box::new(e), Box::new(Expr::Null), Box::new(Expr::Null))),
                            str: x.to_owned(),
                        },
                        i - 1
                    )),

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::Take(b, Box::new(Expr::Null), Box::new(Expr::Null)));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "always" => Ok((Expr::Always, xs)),

                "never" => Ok((Expr::Never, xs)),

                "ranks" => Ok((Expr::CardRank, xs)),

                "suits" => Ok((Expr::CardSuit, xs)),

                "values" => Ok((Expr::CardValue, xs)),

                "discard" => Ok((Expr::Discard, xs)),

                "left" => Ok((Expr::TOLeft, xs)),

                "right" => Ok((Expr::TOLeft, xs)),

                "turn" => Ok((Expr::Turn, xs)),
                
                "goBack" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::GoBack(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::GoBack(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "goForward" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::GoForward(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::GoForward(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "reset" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::Reset(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::Reset(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "player" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::CurrentPlayer(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::CurrentPlayer(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "moves" => Ok((Expr::PMove, xs)),

                "le" => Ok((Expr::CLe, xs)),

                "ge" => Ok((Expr::CGr, xs)),

                "lte" => Ok((Expr::CLEq, xs)),

                "gte" => Ok((Expr::CGRq, xs)),

                "eq" =>  Ok((Expr::CEq, xs)),

                "isMove" => match parse_one_cdsl(xs, i) {
                    Ok((exp, ys)) => {
                        Ok((Expr::IsMove(Box::new(exp)), ys))
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::IsMove(b));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "pPass" => Ok((Expr::PAPass, xs)),

                "pDraw" => Ok((Expr::PADraw, xs)),

                "pPlay" => Ok((Expr::PAPlay, xs)),

                "isSame" => match parse_one_cdsl(xs, i) {
                    Ok((l, ys)) => {
                        match parse_one_cdsl(ys, i - (xs.len() as i32) - (ys.len() as i32)) {
                            Ok((r, zs)) => {
                                Ok((Expr::IsSame(Box::new(l), Box::new(r)), zs))
                            }

                            Err((e, j)) => {
                                let b = match e.get_expr() {
                                    Some(e) => Box::new(e),
                                    None => Box::new(Expr::Null),
                                };
        
                                let expr = Some(Expr::IsSame(Box::new(l), b));
        
                                Err((
                                    ParseError {
                                        err: e.get_error(),
                                        expr,
                                        str: x.to_owned() + " " + &e.get_string(),
                                    },
                                    j
                                ))
                            }
                        }
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::IsSame(b, Box::new(Expr::Null)));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "look" => match parse_one_cdsl(xs, i) {
                    Ok((l, ys)) => {
                        match parse_one_cdsl(ys, i - (xs.len() as i32) - (ys.len() as i32)) {
                            Ok((r, zs)) => {
                                Ok((Expr::Look(Box::new(l), Box::new(r)), zs))
                            }

                            Err((e, j)) => {
                                let b = match e.get_expr() {
                                    Some(e) => Box::new(e),
                                    None => Box::new(Expr::Null),
                                };
        
                                let expr = Some(Expr::Look(Box::new(l), b));
        
                                Err((
                                    ParseError {
                                        err: e.get_error(),
                                        expr,
                                        str: x.to_owned() + " " + &e.get_string(),
                                    },
                                    j
                                ))
                            }
                        }
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::Look(b, Box::new(Expr::Null)));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "put" => match parse_one_cdsl(xs, i) {
                    Ok((l, ys)) => {
                        match parse_one_cdsl(ys, i - (xs.len() as i32) - (ys.len() as i32)) {
                            Ok((r, zs)) => {
                                Ok((Expr::Put(Box::new(l), Box::new(r)), zs))
                            }

                            Err((e, j)) => {
                                let b = match e.get_expr() {
                                    Some(e) => Box::new(e),
                                    None => Box::new(Expr::Null),
                                };
        
                                let expr = Some(Expr::Put(Box::new(l), b));
        
                                Err((
                                    ParseError {
                                        err: e.get_error(),
                                        expr,
                                        str: x.to_owned() + " " + &e.get_string(),
                                    },
                                    j
                                ))
                            }
                        }
                    }

                    Err((e, j)) => {

                        let b = match e.get_expr() {
                            Some(e) => Box::new(e),
                            None => Box::new(Expr::Null),
                        };

                        let expr = Some(Expr::Put(b, Box::new(Expr::Null)));

                        Err((
                            ParseError {
                                err: e.get_error(),
                                expr,
                                str: x.to_owned() + " " + &e.get_string(),
                            },
                            j
                        ))
                    }
                }

                "null" => Ok((Expr::Null, xs)),

                _ => match x.parse::<i32>() {
                    Ok(i) => Ok((Expr::Numeric(i), xs)),
                    Err(_) => Ok((Expr::Text(x.to_owned()), xs)),
                }
            }
        }

        None => Err((ParseError { err: ParseErrorCode::IncompleteExpressionError, expr: None, str: xs.join(" ") }, i)),
    }
    */
}


pub fn get_card_comperator(x: &str) -> Option<Expr> {
    match x {
        "eq" => Some(Expr::CEq),

        "lte" => Some(Expr::CLEq),

        "gte" => Some(Expr::CGRq),

        "le" => Some(Expr::CLe),

        "ge" => Some(Expr::CGr),
        _ => None
    }
}

/**
 * Takes in one line
 */
pub fn read_cdsl(xs: &str) -> Result<((Feature, Option<Vec<Expr>>), Vec<Expr>), (Option<Feature>, Vec<ParseError>)> {
    match xs.find('=') {
        Some(index) => {
            let (y, ys) = xs.split_at(index);
            match Feature::validate_feature(y) {
                Ok(f) => match f {
                    // parse_expr(ys.split_whitespace().collect::<Vec<&str>>())
                    Feature::CEDrawCard => {
                        match y.split_whitespace().collect::<Vec<&str>>()[1].parse::<i32>() {
                            Ok(i) => match parse_expr(ys.split_whitespace().collect::<Vec<&str>>()) {
                                Ok(expr) => Ok(((f, Some(vec![Expr::Numeric(i)])), expr)),
                                Err(e) => Err((Some(f), e))
                            }

                            Err(_) => Err((Some(f), vec![ParseError::new(ParseErrorCode::InvalidFeatureArgumentError, None, y.to_owned())])),
                        }
                    }

                    Feature::CESwapHand => Ok(((f, None), vec![Expr::Cards(get_cards(ys.split_whitespace().collect::<Vec<&str>>()))])),

                    Feature::CEChangeCard => {
                        match get_card_fields(y.split_whitespace().collect::<Vec<&str>>()) {
                            Ok(expr) => match parse_expr(ys.split_whitespace().collect::<Vec<&str>>()) {
                                Ok(cards) => Ok(((f, Some(expr)), cards)),
                                Err(e) => Err((Some(f), e))
                            }

                            Err(mut e) => {
                                e.push(ParseError::new(ParseErrorCode::InvalidFeatureArgumentError, None, y.to_owned()));
                                Err((Some(f), e))
                            }
                        }
                    }

                    Feature::CETakeFromHand => Ok(((f, None), vec![Expr::Cards(get_cards(ys.split_whitespace().collect::<Vec<&str>>()))])),

                    Feature::CEPassNext => Ok(((f, None), vec![Expr::Cards(get_cards(ys.split_whitespace().collect::<Vec<&str>>()))])),

                    Feature::CEGiveCard => Ok(((f, None), vec![Expr::Cards(get_cards(ys.split_whitespace().collect::<Vec<&str>>()))])),

                    Feature::PlayerMoves => Ok(((f, None), ys.split_whitespace().into_iter().filter_map(|x| Move::from_string(x)).map(|(m, b)| Expr::PlayerAction(m, b)).collect::<Vec<Expr>>())),

                    Feature::ExceptionConstraints => match get_card_comperator(y.split_whitespace().collect::<Vec<&str>>()[1]) {
                        Some(cc) => match parse_expr(string_to_list(ys)) {
                            Ok(expr) => Ok(((f, Some(vec![cc])), expr)),
                            Err(e) => Err((Some(f), e)),
                        }
                        None => Err((Some(f), vec![ParseError::new(ParseErrorCode::InvalidFeatureArgumentError, None, y.to_owned())])),
                    }

                    Feature::IgnoreConstraints => Ok(((f, None), vec![Expr::Cards(get_cards(ys.split_whitespace().collect::<Vec<&str>>()))])),
                    
                    f => match parse_expr(string_to_list(xs)) {
                        Ok(expr) => Ok(((f, None), expr)),
                        Err(e) => Err((Some(f), e)),
                    }
                }

                Err(e) => Err((None, vec![ParseError::new(ParseErrorCode::InvalidFeatureArgumentError, None, y.to_owned()), e]))
            }
        }
        None => Err((None, vec![ParseError::new(ParseErrorCode::InvalidFeatureArgumentError, None, xs.to_owned())]))
    }
}