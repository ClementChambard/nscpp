use crate::{
    file_opening::{PPTokType, PPToken},
    parsing::{LitType, TokType, Token},
    preprocessor::{data::PPData, directive::report_extra_tokens},
};

pub fn pp_directive_line(toks: Vec<PPToken>, data: &mut PPData) -> Vec<PPToken> {
    let mut it = toks.into_iter();
    let mut got_line = false;
    let mut extra = vec![];
    for tok in &mut it {
        match &tok.tok {
            PPTokType::Whitespace => {}
            PPTokType::Num(_) => {
                if got_line {
                    extra.push(tok);
                    break;
                }
                let t = Token::from_pptoken(tok).unwrap();
                if t.is_none() {
                    continue;
                }
                let Token { tok, loc } = t.unwrap();
                if let TokType::Lit(LitType::Int(s, _)) = tok {
                    data.line_diff = s - loc.lines.start - 1;
                } else if let TokType::Lit(LitType::Float(s, _)) = tok {
                    data.line_diff = s as usize - loc.lines.start - 1;
                } else {
                    println!("Error in #line directive");
                    return vec![];
                }
                got_line = true;
            }
            PPTokType::Str(_) => {
                if !got_line {
                    println!("Error in #line directive");
                    return vec![];
                }
                let t = Token::from_pptoken(tok).unwrap();
                if t.is_none() {
                    continue;
                }
                let Token { tok: t, loc } = t.unwrap();
                if let TokType::Lit(LitType::String(s, _)) = t {
                    data.cur_file = s;
                } else {
                    extra.push(PPToken {
                        tok: PPTokType::Str(String::from("")),
                        loc,
                    });
                }
                break;
            }
            _ => {
                if !got_line {
                    println!("Error in #line directive");
                    return vec![];
                }
                extra.push(tok);
                break;
            }
        }
    }
    extra.extend(it);
    report_extra_tokens(extra);
    vec![]
}
