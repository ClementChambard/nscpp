use super::data::PPData;
use crate::{file_opening::PPToken, preprocessor::directive::run_directive};

pub fn preprocess_tokens(pptokens: Vec<PPToken>, data: &PPData) -> Vec<PPToken> {
    let mut out = Vec::new();
    for t in pptokens {
        let PPToken { tok, loc } = t;
        // macro expand if necessary
        out.push(PPToken {
            tok,
            loc: loc.w_file_line(&data.cur_file, data.line_diff),
        });
    }
    out
}

pub fn preprocess_file_inner(pptokens: Vec<PPToken>, data: PPData) -> Vec<PPToken> {
    let mut data = data;
    let mut out = Vec::new();
    let mut it = pptokens.into_iter();
    while let Some(t) = it.next() {
        if t.is_punct("#") || t.is_punct("%:") {
            let mut directive = Vec::new();
            for t in &mut it {
                if t.is_newline() {
                    break;
                }
                directive.push(t);
            }
            let directive_out = run_directive(directive, &mut data);
            out.extend(directive_out);
            continue;
        }
        if t.is_newline() {
            continue;
        }
        let mut toks = vec![t];
        for t in &mut it {
            if t.is_newline() {
                break;
            }
            toks.push(t);
        }
        out.extend(preprocess_tokens(toks, &data));
    }
    out
}

pub fn preprocess_file(filename: &str, pptokens: Vec<PPToken>) -> Vec<PPToken> {
    let ppdata = PPData::new(filename);
    preprocess_file_inner(pptokens, ppdata)
}
