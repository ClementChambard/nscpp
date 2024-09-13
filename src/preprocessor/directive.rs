mod conditionnals;
mod defines;
mod fileline;
mod include;
mod messaging;

use crate::{
    file_opening::{PPTokType, PPToken},
    preprocessor::data::PPData,
};
use conditionnals::{
    pp_directive_elif, pp_directive_else, pp_directive_endif, pp_directive_if, IfCondType,
};
use defines::{pp_directive_define, pp_directive_undef};
use fileline::pp_directive_line;
use include::pp_directive_include;
use messaging::{pp_directive_error, pp_directive_warning};

fn report_extra_tokens(toks: Vec<PPToken>) {
    for t in toks {
        if let PPTokType::Whitespace = t.tok {
        } else {
            println!("Extra token at the end of preprocessing directive");
        }
    }
}

pub fn run_directive(toks: Vec<PPToken>, data: &mut PPData) -> Vec<PPToken> {
    let mut toks_iter = toks.into_iter();
    let mut dir = None;
    let mut dir_toks = vec![];
    while let Some(t) = toks_iter.next() {
        match t.tok {
            PPTokType::Whitespace => {}
            PPTokType::Ident(i) => {
                dir = Some(i);
                dir_toks = toks_iter.collect::<Vec<_>>();
                break;
            }
            _ => {
                println!("Error: Invalid preprocessing directive. Skipping.");
                return vec![];
            }
        }
    }
    if dir.is_none() {
        println!("Error: Empty preprocessing directive. Skipping.");
        return vec![];
    }
    let dir = dir.unwrap();

    // always evaluate these directives
    if dir == "endif" {
        return pp_directive_endif(dir_toks, data);
    } else if dir == "else" {
        return pp_directive_else(dir_toks, data);
    } else if dir == "elif" {
        return pp_directive_elif(IfCondType::Expr, dir_toks, data);
    } else if dir == "elifdef" {
        return pp_directive_elif(IfCondType::Def, dir_toks, data);
    } else if dir == "elifndef" {
        return pp_directive_elif(IfCondType::NDef, dir_toks, data);
    }

    if !data.cond_stack.last().unwrap() {
        // In an inactive #if block: do not evaluate directive
        return vec![];
    }

    // only evaluate these directives while in an active code block
    if dir == "if" {
        return pp_directive_if(IfCondType::Expr, dir_toks, data);
    } else if dir == "ifdef" {
        return pp_directive_if(IfCondType::Def, dir_toks, data);
    } else if dir == "ifndef" {
        return pp_directive_if(IfCondType::NDef, dir_toks, data);
    } else if dir == "error" {
        return pp_directive_error(dir_toks, data);
    } else if dir == "warning" {
        return pp_directive_warning(dir_toks, data);
    } else if dir == "undef" {
        return pp_directive_undef(dir_toks, data);
    } else if dir == "define" {
        return pp_directive_define(dir_toks, data);
    } else if dir == "include" {
        return pp_directive_include(dir_toks, data);
    } else if dir == "line" {
        return pp_directive_line(dir_toks, data);
    }
    println!("Error: Invalid preprocessing directive. Skipping.");
    vec![]
}
