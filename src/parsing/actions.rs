use super::{tokens::merge_str_lit, CharType, LitType, TokType, Token};
use crate::{file_opening::PPToken, util::loc::Location};

pub fn translate_tokens(tokens: Vec<PPToken>) -> Vec<Token> {
    let mut out = Vec::new();
    let mut last_str_lit: Option<(String, CharType, Location)> = None;
    let mut had_errors = false;
    for t in tokens {
        let tok = match Token::from_pptoken(t) {
            Ok(t) => t,
            Err(e) => {
                print!("{}", e);
                had_errors = true;
                None
            }
        };
        if tok.is_none() {
            continue;
        }
        let tok = tok.unwrap();
        if let TokType::Lit(LitType::String(s, t)) = tok.tok {
            last_str_lit = Some(if let Some((ls, lt, lloc)) = last_str_lit.take() {
                let (s, t) = merge_str_lit((ls, lt), (s, t));
                (s, t, lloc.merge(tok.loc))
            } else {
                (s, t, tok.loc)
            });
            continue;
        } else if let Some((ls, lt, lloc)) = last_str_lit.take() {
            out.push(Token {
                tok: TokType::Lit(LitType::String(ls, lt)),
                loc: lloc,
            });
        }
        out.push(tok);
    }
    if had_errors {
        panic!("There were some errors while translating tokens. Aborting");
    }
    out
}
