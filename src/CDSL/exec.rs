use crate::CardGame::player::Move;
use crate::{CardGame::game::Game, functions::partition_result};
use crate::CardGame::card::Card;
use super::expr::{Expr, ExecError, CardEffect, ExecErrorCode};

use rand::seq::SliceRandom;

use std::{thread, time};


pub fn place_card_stmt(xs: Vec<Expr>) {

    todo!()
}

pub fn compare_cards<F>(xs: Vec<Card>, g: Game<F>, c: Card) -> bool
where
    F: Fn(Game<F>, Card) -> bool
{
    false
}


pub fn exec_cdsl_game<F>(mut xs: Vec<Expr>, mut g: Game<F>) -> Game<F>
where
    F: Fn(Game<F>, Card) -> bool
{
    match xs.pop() {
        Some(Expr::If(l, r)) => {
            
            if l.into_iter().map(|x| exec_cdsl_game_bool(&x, &g)).filter_map(Result::ok).all(|x| x) {
                g = exec_cdsl_game(r, g);
            }
        }

        Some(Expr::Swap(a, b)) => match (*a, *b) {
            (Expr::Deck, Expr::Pile) => {
                let deck = g.get_deck();
                let pile = g.get_pile();

                g.set_deck(pile.into_iter().map(|(c, _)| c).collect::<Vec<Card>>());

                g.set_pile(deck);

            }

            (Expr::Deck, Expr::Discard) => {
                let deck = g.get_deck();
                let discard = g.get_discard();

                g.set_deck(discard);
                g.set_discard(deck);
            }

            (Expr::Pile, Expr::Deck) => {
                let pile = g.get_pile();
                let deck = g.get_deck();

                g.set_deck(pile.into_iter().map(|(c, _)| c).collect::<Vec<Card>>());
                g.set_pile(deck);
            }

            (Expr::Pile, Expr::Discard) => {
                let pile = g.get_pile();
                let discard = g.get_discard();

                g.set_discard(pile.into_iter().map(|(c, _)| c).collect::<Vec<Card>>());
                g.set_pile(discard);
            }

            (Expr::Discard, Expr::Pile) => {
                let pile = g.get_pile();
                let discard = g.get_discard();

                g.set_discard(pile.into_iter().map(|(c, _)| c).collect::<Vec<Card>>());
                g.set_pile(discard);
            }

            (Expr::Discard, Expr::Deck) => {
                let deck = g.get_deck();
                let discard = g.get_discard();

                g.set_deck(discard);
                g.set_discard(deck);
            }
            
            _ => {}
        }

        Some(Expr::Take(a, b, c)) => match *a {
            Expr::Numeric(n) => {
                let mut f = match *b {
                    Expr::Pile => g.get_pile(),

                    Expr::Deck => g.get_deck().into_iter().map(|c| (c,  None)).collect(),

                    Expr::Discard => g.get_discard().into_iter().map(|c| (c,  None)).collect(),

                    _ => Vec::new()
                };

                let mut t = match *c {
                    Expr::Pile => g.get_pile(),

                    Expr::Deck => g.get_deck().into_iter().map(|c| (c,  None)).collect(),

                    Expr::Discard => g.get_discard().into_iter().map(|c| (c,  None)).collect(),

                    _ => Vec::new()
                };

                for _ in 0..n {
                    match f.pop() {
                        Some(c) => t.push(c),

                        None => break,
                    }
                }

                // to
                match *c {
                    Expr::Pile => g.update_pile(t),

                    Expr::Deck => g.set_deck(t.into_iter().map(|(c, _)| c).collect()),

                    Expr::Discard => g.set_discard(t.into_iter().map(|(c, _)| c).collect()),

                    _ => return exec_cdsl_game(xs, g)
                }

                // from
                match *b {
                    Expr::Pile => g.update_pile(f),

                    Expr::Deck => g.set_deck(f.into_iter().map(|(c, _)| c).collect()),

                    Expr::Discard => g.set_discard(f.into_iter().map(|(c, _)| c).collect()),

                    _ => {}
                }
            }

            _ => {}
        }

        Some(Expr::Shuffle(a)) => match *a {
            Expr::Pile => {
                let mut pile = g.get_pile();

                pile.shuffle(&mut rand::thread_rng());

                g.update_pile(pile);
            },

            Expr::Deck => {
                let mut deck = g.get_deck();

                deck.shuffle(&mut rand::thread_rng());

                g.set_deck(deck);
            }

            Expr::Discard => {
                let mut discard = g.get_discard();

                discard.shuffle(&mut rand::thread_rng());

                g.set_discard(discard);
            }

            _ => {}
        }

        Some(Expr::Reset(a)) => match *a {
            Expr::CurrentPlayer(b) => match *b {
                Expr::PMove => match g.get_players().focus() {
                    Some(mut p) => {
                        p.set_moves(g.get_player_moves());
                        g.update_player(p);
                    },

                    None => { }
                }

                _ => { }
            }

            _ => { }
        }

        Some(Expr::AffectPlayer(ce, x)) => match ce {
            CardEffect::PassNext => {
                match x {
                    Some(mut xs) => {
                        println!("Your turn was passed.");
                        thread::sleep(time::Duration::from_secs(1));
                        let _ = xs.pop();

                        xs.into_iter().map(|e| g.set_turn_order(e));
                    }

                    None => { },
                }
            }


            CardEffect::DrawCards => match (g.get_players().focus(), x) {
                (Some(mut p), Some(arg)) => {
                    let n: i32 = arg.into_iter().filter_map(|e| e.to_numeric()).filter_map(|e| e.get_value()).sum();

                    println!("You must draw {} cards", n);

                    let mut deck = g.get_deck();

                    let mut crds: Vec<Card> = Vec::new();

                    for _ in 0..n {
                        match deck.pop() {
                            Some(c) => crds.push(c),

                            None => break,
                        }
                    }

                    g.set_deck(deck);

                    p.add_to_hand(crds);

                    g.update_player(p);
                }

                _ => { }
            }

            _ => { }
        }

        Some(Expr::TOLeft) => {
            let mut players = g.get_players();
            players.rot_l();
            g.set_players(players);
        }

        Some(Expr::TORight) => {
            let mut players = g.get_players();
            players.rot_r();
            g.set_players(players);
        }

        Some(Expr::Put(l, r)) => {
            let mut f = match *l {
                Expr::Pile => g.get_pile(),

                Expr::Deck => g.get_deck().into_iter().map(|c| (c,  None)).collect(),

                Expr::Discard => g.get_discard().into_iter().map(|c| (c,  None)).collect(),

                _ => Vec::new()
            };

            let mut t = match *r {
                Expr::Pile => g.get_pile(),

                Expr::Deck => g.get_deck().into_iter().map(|c| (c,  None)).collect(),

                Expr::Discard => g.get_discard().into_iter().map(|c| (c,  None)).collect(),

                _ => Vec::new()
            };

            t.append(&mut f);


            // to
            match *r {
                Expr::Pile => g.update_pile(t),

                Expr::Deck => g.set_deck(t.into_iter().map(|(c, _)| c).collect()),

                Expr::Discard => g.set_discard(t.into_iter().map(|(c, _)| c).collect()),

                _ => return exec_cdsl_game(xs, g)
            }

            // from
            match *l {
                Expr::Pile => g.update_pile(f),

                Expr::Deck => g.set_deck(f.into_iter().map(|(c, _)| c).collect()),

                Expr::Discard => g.set_discard(f.into_iter().map(|(c, _)| c).collect()),

                _ => { }
            }

        }

        Some(Expr::GoBack(a)) => match *a {
            Expr::Turn => match g.get_turn_order() {
                Expr::TOLeft => {
                    let mut players = g.get_players();
                    players.rot_r();
                    g.set_players(players);
                }

                Expr::TORight => {
                    let mut players = g.get_players();
                    players.rot_l();
                    g.set_players(players);
                }

                _ => { }
            }

            _ => { }
        }

        Some(Expr::GoForward(a)) => match *a {
            Expr::Turn => match g.get_turn_order() {
                Expr::TOLeft => {
                    let mut players = g.get_players();
                    players.rot_l();
                    g.set_players(players);
                }

                Expr::TORight => {
                    let mut players = g.get_players();
                    players.rot_r();
                    g.set_players(players);
                }

                _ => { }
            }

            _ => { }
        }

        Some(_) => return exec_cdsl_game(xs, g),
        
        None => return g
    }

    return exec_cdsl_game(xs, g);
}


pub fn exec_cdsl_bool(x: &Expr) -> Result<bool, ExecError> {
    match x.clone() {
        Expr::Always => Ok(true),

        Expr::Never => Ok(false),

        Expr::IsEqual(a, b) => match (*a, *b) {
            (Expr::Numeric(l), Expr::Numeric(r)) => Ok(l == r),

            _ => Err(ExecError::new(ExecErrorCode::InvalidBoolEvaluationError, x.clone()))
        }

        Expr::Not(ex) => {
            let (res, mut errs) = partition_result(ex.iter().map(|expr| exec_cdsl_bool(expr)).collect::<Vec<Result<bool, ExecError>>>());

            if !errs.is_empty() {
                return Err(errs.pop().unwrap());
            }

            return Ok(res.into_iter().all(|b| !b));
        }

        Expr::Or(l, r) => {
            let (l_res, mut l_err) = partition_result(l.iter().map(|expr| exec_cdsl_bool(expr)).collect::<Vec<Result<bool, ExecError>>>());
            let (r_res, mut r_err) = partition_result(r.iter().map(|expr| exec_cdsl_bool(expr)).collect::<Vec<Result<bool, ExecError>>>());

            if !l_err.is_empty() {
                return Err(l_err.pop().unwrap());
            } else if !r_err.is_empty() {
                return Err(r_err.pop().unwrap());
            }

            Ok(l_res.into_iter().all(|b| b) || r_res.into_iter().all(|b| b))
        }

        Expr::And(l, r) => {
            let (l_res, mut l_err) = partition_result(l.iter().map(|expr| exec_cdsl_bool(expr)).collect::<Vec<Result<bool, ExecError>>>());
            let (r_res, mut r_err) = partition_result(r.iter().map(|expr| exec_cdsl_bool(expr)).collect::<Vec<Result<bool, ExecError>>>());

            if !l_err.is_empty() {
                return Err(l_err.pop().unwrap());
            } else if !r_err.is_empty() {
                return Err(r_err.pop().unwrap());
            }

            Ok(l_res.into_iter().all(|b| b) && r_res.into_iter().all(|b| b))
        }

        _ => Err(ExecError::new(ExecErrorCode::InvalidBoolEvaluationError, x.clone()))
    }
}


pub fn exec_cdsl_game_bool<F>(x: &Expr, g: &Game<F>) -> Result<bool, ExecError>
where
    F: Fn(Game<F>, Card) -> bool
{
    match x {
        Expr::IsEmpty(a) => match **a {
            Expr::Deck => Ok(g.get_deck().is_empty()),

            Expr::Discard => Ok(g.get_deck().is_empty()),

            Expr::Pile => Ok(g.get_deck().is_empty()),

            _ => Err(ExecError::new(ExecErrorCode::InvalidBoolEvaluationError, x.clone()))
        }

        Expr::IsEqual(a, b) => todo!(),

        Expr::Any(a) => todo!(),

        Expr::All(a) => todo!(),

        Expr::IsSame(a, b) => todo!(),

        Expr::CurrentPlayer(a) => match *(a.clone()) {
            Expr::IsMove(b) => match *b {
                Expr::PADraw => todo!(),

                Expr::PAPlay => todo!(),

                Expr::PAPass => match g.get_players().focus() {
                    Some(p) => {
                        match p.get_move_history().pop() {
                            Some((m, _)) => Ok(m == Move::Pass),

                            None => Ok(false),
                        }
                    }

                    None => Ok(false),
                }

                _ => Err(ExecError::new(ExecErrorCode::InvalidSyntaxError, x.clone()))
            }

            _ => Err(ExecError::new(ExecErrorCode::InvalidSyntaxError, x.clone()))
        }

        Expr::PreviousPlayer(a) => match *(a.clone()) {
            Expr::IsMove(b) => match *b {
                Expr::PADraw => todo!(),

                Expr::PAPlay => todo!(),

                Expr::PAPass => match g.get_players().focus() {
                    Some(p) => {
                        match p.get_move_history().pop() {
                            Some((m, _)) => Ok(m == Move::Pass),

                            None => Ok(false),
                        }
                    }

                    None => Ok(false),
                }

                _ => Err(ExecError::new(ExecErrorCode::InvalidSyntaxError, x.clone()))
            }

            _ => Err(ExecError::new(ExecErrorCode::InvalidSyntaxError, x.clone()))
        }

        _ => exec_cdsl_bool(x)
    }
}