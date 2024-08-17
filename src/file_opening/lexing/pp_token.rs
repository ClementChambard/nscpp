use super::super::SourceLine;
use super::Location;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PPTokType {
    Ident(String),
    Num(String),
    Chr(String),
    Str(String),
    Punct(String),
    Other(char),
    Whitespace,
    NewLine,
    HeaderName(String),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PPToken<'a> {
    pub tok: PPTokType,
    pub loc: Location<'a>,
}

// TODO: HeaderName
pub fn parse_one<'a, 'b>(
    filename: &'a str,
    source: &'b [SourceLine],
    col: usize,
) -> (&'b [SourceLine], usize, PPToken<'a>) {
    let mut source = source;
    let mut source_iter = source.iter();
    let mut next = source_iter.next().unwrap();
    while let SourceLine::Join(_) = next {
        source = &source[1..];
        next = source_iter.next().unwrap();
    }
    let SourceLine::Line(line_num, line_str) = next else {
        unreachable!();
    };
    let line_str = &line_str[col..];
    if line_str.is_empty() {
        return (
            &source[1..],
            0,
            PPToken {
                tok: PPTokType::NewLine,
                loc: Location::new(filename, *line_num, col),
            },
        );
    }
    if let Some(res) = parse_whitespace(filename, source, col) {
        return res;
    }
    if let Some(res) = parse_string(filename, source, col) {
        return res;
    }
    let cnt;
    let tok;
    if let Some(res) = parse_char(line_str) {
        (cnt, tok) = res;
    } else if let Some(res) = parse_number(line_str) {
        (cnt, tok) = res;
    } else if let Some(res) = parse_ident(line_str) {
        (cnt, tok) = res;
    } else if let Some(res) = parse_punctuator(line_str) {
        (cnt, tok) = res;
    } else {
        (cnt, tok) = (1, PPTokType::Other(line_str.chars().next().unwrap()));
    }

    (
        source,
        col + cnt,
        PPToken {
            tok,
            loc: Location::lrange(filename, *line_num, col..col + cnt - 1),
        },
    )
}

fn parse_whitespace<'a, 'b>(
    filename: &'b str,
    lines: &'a [SourceLine],
    col: usize,
) -> Option<(&'a [SourceLine], usize, PPToken<'b>)> {
    let mut lines = lines;
    let mut lines_iter = lines.iter();
    let next = lines_iter.next()?;
    let SourceLine::Line(line_num, line_str) = next else {
        unreachable!();
    };
    let mut line_str = &line_str[col..];

    if line_str.starts_with("//") {
        lines = &lines[1..];
        let mut line_cnt = 1;
        for l in lines_iter.step_by(2) {
            match l {
                SourceLine::Line(_, _) => break,
                _ => {
                    lines = &lines[2..];
                    line_cnt += 1;
                }
            }
        }
        return Some((
            lines,
            0,
            PPToken {
                tok: PPTokType::NewLine,
                loc: Location::range(filename, *line_num..*line_num + line_cnt, col..0),
            },
        ));
    }

    if line_str.starts_with("/*") {
        let mut last_line = line_num;
        line_str = &line_str[2..];
        let first_col = col;
        let mut col = col + 2;
        loop {
            if line_str.is_empty() {
                lines = &lines[1..];
                let mut next = lines_iter
                    .next()
                    .expect("not closed multiline comment found");
                while let SourceLine::Join(_) = next {
                    next = lines_iter
                        .next()
                        .expect("not closed multiline comment found");
                    lines = &lines[1..];
                }
                let SourceLine::Line(last_line_o, line_str_o) = next else {
                    unreachable!();
                };
                last_line = last_line_o;
                line_str = line_str_o;
                col = 0;
                continue;
            }
            if line_str.starts_with("*/") {
                col += 2;
                break;
            } else {
                line_str = &line_str[1..];
                col += 1;
            }
        }
        return Some((
            lines,
            col,
            PPToken {
                tok: PPTokType::Whitespace,
                loc: Location::range(filename, *line_num..*last_line, first_col..col),
            },
        ));
    }

    let c = line_str.chars().next()?;
    if !c.is_whitespace() {
        return None;
    }
    let start_col = col;
    let mut col = col + 1;
    let mut has_nl = false;
    let mut last_line = line_num;
    line_str = &line_str[1..];

    loop {
        if line_str.is_empty() {
            lines = &lines[1..];
            let mut next = lines_iter.next();
            let mut nl = true;
            while let Some(SourceLine::Join(_)) = next {
                next = lines_iter.next();
                lines = &lines[1..];
                nl = false;
            }
            if next.is_none() {
                break;
            }
            let SourceLine::Line(last_line_o, line_str_o) = next.unwrap() else {
                unreachable!();
            };
            last_line = last_line_o;
            line_str = line_str_o;
            has_nl = has_nl || nl;
            col = 0;
            continue;
        }
        if line_str.chars().next().unwrap().is_whitespace() {
            line_str = &line_str[1..];
            col += 1;
        } else {
            break;
        }
    }

    Some((
        lines,
        col,
        PPToken {
            tok: if has_nl {
                PPTokType::NewLine
            } else {
                PPTokType::Whitespace
            },
            loc: Location::range(filename, *line_num..*last_line, start_col..col),
        },
    ))
}

fn is_ident_start(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '_' => true,
        _ => unicode_xid::UnicodeXID::is_xid_start(c),
    }
}

fn is_ident_continue(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true,
        _ => unicode_xid::UnicodeXID::is_xid_continue(c),
    }
}

fn parse_ident(s: &str) -> Option<(usize, PPTokType)> {
    let mut chars = s.chars();
    let mut cnt = 0;
    let c = chars.next()?;
    if !is_ident_start(c) {
        return None;
    }
    cnt += 1;
    for c in chars {
        if is_ident_continue(c) {
            cnt += 1;
        } else {
            break;
        }
    }
    Some((cnt, PPTokType::Ident(String::from(&s[..cnt]))))
}

fn parse_number(s: &str) -> Option<(usize, PPTokType)> {
    let mut chars = s.chars();
    let mut cnt = 0;
    let c = chars.next()?;
    match c {
        '0'..='9' => {
            cnt += 1;
        }
        '.' => {
            let c = chars.next()?;
            match c {
                '0'..='9' => cnt += 2,
                _ => return None,
            }
        }
        _ => return None,
    }
    let mut exponent = false;
    while let Some(c) = chars.next() {
        match c {
            'P' | 'p' | 'e' | 'E' => {
                cnt += 1;
                exponent = true;
            }
            '+' | '-' => {
                if !exponent {
                    break;
                }
                exponent = false;
                cnt += 1;
            }
            '.' => {
                exponent = false;
                cnt += 1;
            }
            c if is_ident_continue(c) => {
                exponent = false;
                cnt += 1;
            }
            '\'' => {
                exponent = false;
                let c = chars.next();
                if c.is_none() {
                    break;
                }
                match c.unwrap() {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => cnt += 2,
                    _ => break,
                }
            }
            _ => break,
        }
    }

    Some((cnt, PPTokType::Num(String::from(&s[..cnt]))))
}

fn read_escape_seq(_text: &str) -> Option<(&str, usize)> {
    todo!("read escape seq");
}

fn read_s_char(text: &str) -> Option<(&str, usize)> {
    match text.chars().next()? {
        '"' | '\n' => None,
        '\\' => read_escape_seq(text),
        _ => Some((&text[..1], 1)),
    }
}

fn _read_d_char(text: &str) -> Option<(&str, usize)> {
    let c = text.chars().next()?;
    if c.is_whitespace() || c == '\\' || c == '(' || c == ')' {
        None
    } else {
        Some((&text[..1], 1))
    }
}

fn read_c_char(text: &str) -> Option<(&str, usize)> {
    match text.chars().next()? {
        '\'' | '\n' => None,
        '\\' => read_escape_seq(text),
        _ => Some((&text[..1], 1)),
    }
}

fn parse_raw_string<'a, 'b>(
    _filename: &'a str,
    _lines: &'b [SourceLine],
    _col: usize,
    _start: String,
) -> Option<(&'b [SourceLine], usize, PPToken<'a>)> {
    unimplemented!("parse_raw_string: TODO");
}

fn parse_string<'a, 'b>(
    filename: &'b str,
    lines: &'a [SourceLine],
    col: usize,
) -> Option<(&'a [SourceLine], usize, PPToken<'b>)> {
    let mut lines = lines;
    let mut lines_iter = lines.iter();
    let next = lines_iter.next()?;
    let SourceLine::Line(line_num, line_str) = next else {
        unreachable!();
    };
    let mut line_str = &line_str[col..];
    let start_col = col;
    let mut col = col;

    let mut out = String::new();
    if line_str.starts_with('"') {
        out.push('"');
        line_str = &line_str[1..];
        col += 1;
    } else if line_str.starts_with("R\"") {
        out.push_str(&line_str[..2]);
        return parse_raw_string(filename, lines, col + 2, out);
    } else if line_str.starts_with("L\"")
        || line_str.starts_with("u\"")
        || line_str.starts_with("U\"")
    {
        out.push_str(&line_str[..2]);
        line_str = &line_str[2..];
        col += 2;
    } else if line_str.starts_with("LR\"")
        || line_str.starts_with("uR\"")
        || line_str.starts_with("UR\"")
    {
        out.push_str(&line_str[..3]);
        return parse_raw_string(filename, lines, col + 3, out);
    } else if line_str.starts_with("u8\"") {
        out.push_str(&line_str[..3]);
        line_str = &line_str[3..];
        col += 3;
    } else if line_str.starts_with("u8R\"") {
        out.push_str(&line_str[..4]);
        return parse_raw_string(filename, lines, col + 4, out);
    } else {
        return None;
    }

    let mut last_line = *line_num;
    loop {
        if line_str.is_empty() {
            match lines_iter.next() {
                Some(SourceLine::Join(_)) => {}
                _ => break,
            }
            line_str = match lines_iter.next() {
                Some(SourceLine::Line(_, l)) => l,
                _ => break,
            };
            last_line += 1;
            lines = &lines[2..];
            col = 0;
        }
        if let Some((c, cnt)) = read_s_char(line_str) {
            out.push_str(c);
            line_str = &line_str[cnt..];
            col += cnt;
        } else {
            break;
        }
    }

    if !line_str.starts_with('"') {
        panic!("missing closing '\"' character in string litteral");
    }
    out.push('"');

    Some((
        lines,
        col + 1,
        PPToken {
            tok: PPTokType::Str(out),
            loc: Location::range(filename, *line_num..last_line, start_col..col),
        },
    ))
}

fn parse_char(text: &str) -> Option<(usize, PPTokType)> {
    let mut out = String::new();
    let mut cnt = 0;
    let mut text = text;
    // TODO: can_multi
    // let mut can_multi = false;
    if text.starts_with('\'') {
        out.push('\'');
        text = &text[1..];
        cnt += 1;
        // can_multi = true;
    } else if text.starts_with("L'") || text.starts_with("u'") || text.starts_with("U'") {
        out.push_str(&text[..2]);
        text = &text[2..];
        cnt += 2;
    } else if text.starts_with("u8'") {
        out.push_str(&text[..3]);
        text = &text[3..];
        cnt += 3;
    } else {
        return None;
    }

    let Some((c, c_cnt)) = read_c_char(text) else {
        panic!("invalid c-char in char litteral");
    };
    out.push_str(c);
    cnt += c_cnt;
    text = &text[c_cnt..];

    if !text.starts_with('\'') {
        panic!("missing closing ''' character in char litteral");
    }
    out.push('\'');
    cnt += 1;
    Some((cnt, PPTokType::Chr(out)))
}

const PP_PUNCTUATOR_LIST: &[&str] = &[
    "...", "::", ".*", "->*", "->", "+=", "-=", "*=", "/=", "%=", "^=", "&=", "|=", "==", "!=",
    "<=>", "<=", ">=", ">>=", "<<=", ">>", "<<", "&&", "||", "++", "--", "##", "%:%:", "#", "{",
    "<%", "}", "%>", "[", "<:", "]", ":>", "(", ")", ";", ":", "?", ".", "~", "!", "+", "-", "*",
    "/", "%", "^", "&", "|", "=", "<", ">", ",",
];

fn parse_punctuator(text: &str) -> Option<(usize, PPTokType)> {
    if text.starts_with("<::") && !text.starts_with("<:::") && !text.starts_with("<::>") {
        return Some((1, PPTokType::Punct(String::from("<"))));
    }
    for punct in PP_PUNCTUATOR_LIST {
        if text.starts_with(punct) {
            return Some((punct.len(), PPTokType::Punct(String::from(*punct))));
        }
    }
    None
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn parse_string_test() {
        let source_lines = &[
            SourceLine::Line(0, String::from("  \"te")),
            SourceLine::Join(String::from("\\")),
            SourceLine::Line(1, String::from("st\" aa")),
            SourceLine::Line(2, String::from("line 2")),
        ];
        let res = parse_string("testfilename", source_lines, 2);
        assert_eq!(
            res,
            Some((
                &source_lines[2..],
                3,
                PPToken {
                    tok: PPTokType::Str(String::from("\"test\"")),
                    loc: Location::new("testfilename", 0, 2)
                }
            ))
        );
    }
    #[test]
    fn parse_whitespace_test() {
        let source_lines = &[
            SourceLine::Line(0, String::from(" a")),
            SourceLine::Line(1, String::from("   */ b")),
            SourceLine::Join(String::from("\\")),
            SourceLine::Line(2, String::from("*/ a")),
            SourceLine::Line(3, String::from("line 2")),
        ];
        let res = parse_whitespace("testfilename", source_lines, 0);
        assert_eq!(
            res,
            Some((
                &source_lines[0..],
                1,
                PPToken {
                    tok: PPTokType::Whitespace,
                    loc: Location::range("testfilename", 0..0, 0..1)
                }
            ))
        );
    }
}