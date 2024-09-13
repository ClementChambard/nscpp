use super::{parse_one, PPTokType, PPToken};

pub fn read_source(source: &str, filename: &str) -> Vec<PPToken> {
    let source = super::translation(String::from(source));
    let lines = super::line_join(source);
    let mut lines_ref = &lines[..];
    let mut col = 0;
    let mut tokens = Vec::<PPToken>::new();

    let mut inside_ppdir = false;
    let mut last_is_hash = false;
    let mut last_is_hasinclude = false;
    let mut inside_include_ppdir = false;
    let mut inside_has_include = false;
    let mut headername_allowed = false;
    let mut par_stack = 0;
    while !lines_ref.is_empty() {
        let (out_lines, out_col, out_tok) = parse_one(filename, lines_ref, col, headername_allowed);

        // Headername logic
        if !inside_ppdir {
            if let PPTokType::Punct(p) = &out_tok.tok {
                if p == "#" {
                    last_is_hash = true;
                    inside_ppdir = true;
                }
            }
        } else {
            match &out_tok.tok {
                PPTokType::NewLine => {
                    inside_ppdir = false;
                    inside_has_include = false;
                    last_is_hasinclude = false;
                    inside_include_ppdir = false;
                    headername_allowed = false;
                }
                PPTokType::Whitespace => {}
                PPTokType::Punct(p) => {
                    if p == "(" {
                        if last_is_hasinclude {
                            inside_has_include = true;
                        }
                        if inside_has_include {
                            headername_allowed = true;
                            par_stack += 1;
                        }
                    } else if p == ")" && inside_has_include {
                        par_stack -= 1;
                        if par_stack == 0 && !inside_include_ppdir {
                            headername_allowed = false;
                        }
                    }
                    last_is_hasinclude = false;
                }
                PPTokType::Ident(i) => {
                    if last_is_hash && i == "include" {
                        headername_allowed = true;
                        inside_include_ppdir = true;
                    } else {
                        last_is_hasinclude = i == "__has_include";
                    }
                }
                _ => {
                    last_is_hasinclude = false;
                }
            }
            last_is_hash = false;
        }

        lines_ref = out_lines;
        col = out_col;
        tokens.push(out_tok);
    }
    tokens
}

pub fn open_file(filename: &str) -> Vec<PPToken> {
    let source = std::fs::read_to_string(filename).unwrap();
    read_source(&source, filename)
}
