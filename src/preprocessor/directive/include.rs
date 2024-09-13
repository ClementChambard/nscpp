use crate::{
    file_opening::{PPTokType, PPToken},
    preprocessor::{
        actions::preprocess_tokens,
        data::{PPData, PPMacro},
    },
};

fn get_file_tokens(filename: &str, defines: &[PPMacro]) -> Vec<PPToken> {
    let tokens = crate::file_opening::open_file(filename);
    let data = PPData::with_macros(filename, defines);
    crate::preprocessor::actions::preprocess_file_inner(tokens, data)
}

enum HeaderType {
    Angle,
    Quote,
}

fn get_header_name(toks: Vec<PPToken>, data: &PPData) -> (HeaderType, String) {
    // TODO: Warn extra tokens
    for t in toks.iter() {
        match &t.tok {
            PPTokType::HeaderName(n) => {
                let ty = if n.starts_with('<') {
                    HeaderType::Angle
                } else if n.starts_with('"') {
                    HeaderType::Quote
                } else {
                    unreachable!();
                };
                let hname = n[1..n.len() - 1].to_owned();
                return (ty, hname);
            }
            PPTokType::Whitespace => {}
            _ => {
                break;
            }
        }
    }
    let toks = preprocess_tokens(toks, data);
    // -> StrLit: check if q-char-seq
    // -> Starts with '<' token: stringify until '>' token, check if h-char-seq
    let mut angle_hname = String::new();
    let mut in_angle = false;
    for t in toks.iter() {
        match &t.tok {
            PPTokType::HeaderName(n) => {
                if !in_angle {
                    let ty = if n.starts_with('<') {
                        HeaderType::Angle
                    } else if n.starts_with('"') {
                        HeaderType::Quote
                    } else {
                        unreachable!();
                    };
                    let hname = n[1..n.len() - 1].to_owned();
                    return (ty, hname);
                }
            }
            PPTokType::Str(s) => {
                if !in_angle {
                    if !s.starts_with('"') {
                        panic!("Error invalid string litteral in headername");
                    }
                    let hname = s[1..s.len() - 1].to_owned();
                    return (HeaderType::Quote, hname);
                }
            }
            PPTokType::Punct(p) => {
                if p == "<" && !in_angle {
                    in_angle = true;
                    continue;
                } else if in_angle && p == ">" {
                    break;
                } else if !in_angle {
                    panic!("Error in headername");
                }
            }
            PPTokType::Whitespace => {}
            _ => {
                if !in_angle {
                    panic!("Error in headername");
                }
            }
        }
        if in_angle {
            angle_hname.push_str(&t.tok.stringify());
        }
    }
    (HeaderType::Angle, angle_hname)
}

pub fn pp_directive_include(toks: Vec<PPToken>, data: &mut PPData) -> Vec<PPToken> {
    let (_hty, hname) = get_header_name(toks, data);
    // TODO: use hty to find file
    get_file_tokens(&hname, &data.macros)
}
