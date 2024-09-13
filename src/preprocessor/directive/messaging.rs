use crate::{
    file_opening::{PPTokType, PPToken},
    preprocessor::data::PPData,
};

fn get_message(toks: Vec<PPToken>) -> String {
    let mut out = String::new();
    for t in toks {
        match t.tok {
            PPTokType::Punct(s)
            | PPTokType::Chr(s)
            | PPTokType::Str(s)
            | PPTokType::Ident(s)
            | PPTokType::Num(s)
            | PPTokType::HeaderName(s) => out.push_str(&s),
            PPTokType::Other(c) => out.push(c),
            PPTokType::Whitespace => out.push(' '),
            PPTokType::NewLine => out.push('\n'),
        }
    }
    String::from(out.trim())
}

pub fn pp_directive_error(toks: Vec<PPToken>, _data: &mut PPData) -> Vec<PPToken> {
    println!("error: #error {}", get_message(toks));
    vec![]
}

pub fn pp_directive_warning(toks: Vec<PPToken>, _data: &mut PPData) -> Vec<PPToken> {
    println!("warning: #warning {}", get_message(toks));
    vec![]
}
