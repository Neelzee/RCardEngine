use std::collections::HashMap;
use std::fs::read_to_string;

use crate::CDSL::expr::ParseErrorCode;
use crate::CDSL::parser::read_cdsl;
use crate::{feature::{Attribute, Feature}, CDSL::expr::{Expr, ParseError}};

pub struct GameData {
    data: HashMap<Attribute, HashMap<Feature, (Option<Vec<Expr>>, Vec<Expr>)>>
}

impl GameData {
    pub fn load(file_path: &str) -> Result<GameData, (ParseError, i32)> {
        match read_to_string(file_path) {
            Ok(contents) => {

                let mut attributes: Vec<(Attribute, String, i32, i32)> = Vec::new();
                
                let mut wrd = String::new();

                let mut acc = String::new();

                let mut line = 1;

                let mut start = 0;

                for c in contents.chars() {
                    match &c {
                        // The accumilated chars, should be an attribute
                        '{' => {
                            wrd = acc.clone();
                            acc.clear();
                            start = line.clone();
                        }
                        
                        // The accumilated chars should be the contents of the attribute
                        '}' => match Attribute::from_string(&wrd.trim()) {
                            Some(att) => {
                                attributes.push((att, acc.clone(), start, line));
                                acc.clear();
                                wrd.clear();
                            }

                            None => return Err((ParseError::new(ParseErrorCode::NotAnAttributeError, None, wrd.trim().to_owned()), line))
                        } 

                        // New line
                        '\n' => line += 1,

                        // All chars added to the accumilator
                        _ => acc.push(c)
                    }
                }

                let mut gd: HashMap<Attribute, HashMap<Feature, (Option<Vec<Expr>>, Vec<Expr>)>> = HashMap::new();

                for (att, content, s, _) in attributes {

                    let mut inner: HashMap<Feature, (Option<Vec<Expr>>, Vec<Expr>)> = HashMap::new();

                    for xs in content.split('\n') {
                        match read_cdsl(xs) {
                            Ok(((f, arg), expr)) => {
                                if f.is_feature_of(&att) {
                                    inner.insert(f, (arg, expr));
                                } else {
                                    return Err((ParseError::new(ParseErrorCode::NotAFeatureOfAttributeError, None, xs.to_owned()), s));
                                }
                            }

                            Err((_f, _er)) => return Err((ParseError::new(ParseErrorCode::SyntaxError, None, xs.to_owned()), s))
                        }
                    }
                    gd.insert(att, inner);
                }


                return Ok(GameData { data: gd });
            }

            Err(_) => Err((ParseError::new(ParseErrorCode::InvalidFilePath, None, file_path.to_owned()), 0))
        }
    }
}