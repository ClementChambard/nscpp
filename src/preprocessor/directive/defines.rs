use crate::{
    file_opening::{PPTokType, PPToken},
    preprocessor::data::PPData,
};

pub fn pp_directive_define(_toks: Vec<PPToken>, _data: &mut PPData) -> Vec<PPToken> {
    // TODO:
    vec![]
}

pub fn pp_directive_undef(toks: Vec<PPToken>, data: &mut PPData) -> Vec<PPToken> {
    let mut n = None;
    for t in toks.into_iter() {
        match t.tok {
            PPTokType::Whitespace => continue,
            PPTokType::Ident(m) => {
                n = Some(m);
                break;
            }
            _ => panic!("Error in #undef directive: macro name should be an identifier"),
        }
    }
    if n.is_none() {
        panic!("Missing macro name in #undef directive");
    }
    // report extra tokens
    let n = n.unwrap();
    data.macros.retain(|m| m.name != n);
    vec![]
}
