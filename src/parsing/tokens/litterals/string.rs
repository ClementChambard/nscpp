use super::char::{read_one_char_or_escape_sequence, read_type_prefix, CharType};

pub fn merge_str_lit(s1: (String, CharType), s2: (String, CharType)) -> (String, CharType) {
    if s1.1 == CharType::Char {
        (s1.0 + &s2.0, s2.1)
    } else if s2.1 == CharType::Char || s2.1 == s1.1 {
        (s1.0 + &s2.0, s1.1)
    } else {
        crate::ErrorBuilder::new()
            .error("Can't merge string litteral of different types")
            .message(&format!("Types are {:?} and {:?}", s1.1, s2.1))
            .panic();
    }
}

fn read_raw(s: &str) -> Result<String, &'static str> {
    let s = s
        .strip_prefix('"')
        .ok_or("missing opening \"")?
        .strip_suffix('"')
        .ok_or("missing closing \"")?;
    let mut prefix = String::new();
    for c in s.chars() {
        if c == '(' {
            break;
        }
        prefix.push(c);
    }
    let suffix = format!("){}", prefix);
    let s = s
        .strip_prefix(&prefix)
        .ok_or("invalid raw string prefix")?
        .strip_prefix('(')
        .ok_or("missing ( in raw string")?
        .strip_suffix(&suffix)
        .ok_or("invalid raw string suffix")?;
    Ok(String::from(s))
}

pub fn read(s: String) -> Result<(CharType, String), &'static str> {
    let (ty, s) = read_type_prefix(&s);
    if let Some(s) = s.strip_prefix('R') {
        return Ok((ty, read_raw(s)?));
    }
    let mut s = s
        .strip_prefix('"')
        .ok_or("missing opening \"")?
        .strip_suffix('"')
        .ok_or("missing closing \"")?;
    let mut out = String::new();
    while !s.is_empty() {
        let (c, new_s) = read_one_char_or_escape_sequence(s)?;
        s = new_s;
        out.push(c);
    }
    Ok((ty, out))
}
