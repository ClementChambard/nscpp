use crate::{
    file_opening::{PPTokType, PPToken},
    preprocessor::{data::PPData, directive::report_extra_tokens},
};

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum IfCondType {
    Def,
    NDef,
    Expr,
}

// TODO:
fn is_def(_m: &str, _data: &PPData) -> bool {
    false
}

fn evaluate_define(toks: Vec<PPToken>, data: &PPData, invert: bool) -> bool {
    let mut n = None;
    for t in toks.into_iter() {
        match t.tok {
            PPTokType::Whitespace => continue,
            PPTokType::Ident(m) => {
                n = Some(m);
                break;
            }
            _ => panic!("Error in #ifdef directive: macro name should be an identifier"),
        }
    }
    // report extra tokens
    if n.is_none() {
        panic!("Missing macro name in #ifdef directive");
    }
    let r = is_def(&n.unwrap(), data);
    if invert {
        !r
    } else {
        r
    }
}

fn evaluate_condition(ty: IfCondType, toks: Vec<PPToken>, data: &PPData) -> bool {
    if ty != IfCondType::Expr {
        return evaluate_define(toks, data, ty == IfCondType::NDef);
    }
    // Condition is an expression
    false
}

pub fn pp_directive_endif(toks: Vec<PPToken>, data: &mut PPData) -> Vec<PPToken> {
    data.cond_stack.pop();
    if data.cond_stack.is_empty() {
        panic!("misplaced #endif");
    }
    report_extra_tokens(toks);
    vec![]
}

pub fn pp_directive_else(toks: Vec<PPToken>, data: &mut PPData) -> Vec<PPToken> {
    if data.cond_stack.len() == 1 {
        panic!("misplaced #else");
    }
    let a = data.cond_stack.pop().expect("error in #else directive");
    data.cond_stack.push(a);
    report_extra_tokens(toks);
    vec![]
}

pub fn pp_directive_elif(ty: IfCondType, toks: Vec<PPToken>, data: &mut PPData) -> Vec<PPToken> {
    if data.cond_stack.len() == 1 {
        panic!("misplaced #elif");
    }
    let a = data.cond_stack.pop().expect("error in #elif directive");
    if a {
        data.cond_stack.push(false);
    } else {
        data.cond_stack.push(evaluate_condition(ty, toks, data));
    }
    vec![]
}

pub fn pp_directive_if(ty: IfCondType, toks: Vec<PPToken>, data: &mut PPData) -> Vec<PPToken> {
    data.cond_stack.push(evaluate_condition(ty, toks, data));
    vec![]
}
