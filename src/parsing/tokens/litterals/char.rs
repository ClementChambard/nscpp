#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CharType {
    Char,
    WChar,
    Char8,
    Char16,
    Char32,
}

impl CharType {
    pub fn get_str(&self) -> &'static str {
        match self {
            Self::Char => "",
            Self::WChar => "L",
            Self::Char8 => "u8",
            Self::Char16 => "u",
            Self::Char32 => "U",
        }
    }
}

pub fn read_type_prefix(s: &str) -> (CharType, &str) {
    if let Some(s) = s.strip_prefix('L') {
        (CharType::WChar, s)
    } else if let Some(s) = s.strip_prefix("u8") {
        (CharType::Char8, s)
    } else if let Some(s) = s.strip_prefix('u') {
        (CharType::Char16, s)
    } else if let Some(s) = s.strip_prefix('U') {
        (CharType::Char32, s)
    } else {
        (CharType::Char, s)
    }
    // There should not be any other configuration. No errors
}

type CharResult<'a> = Result<(char, &'a str), &'static str>;

fn read_onum_escape_seq(_s: &str) -> CharResult {
    Err("Octal escape sequences are not implemented.")
}

fn read_o_escape_seq(_s: &str) -> CharResult {
    Err("Octal escape sequences are not implemented.")
}

fn read_x_escape_seq(_s: &str) -> CharResult {
    Err("Hex escape sequences are not implemented.")
}

fn read_u_escape_seq(_s: &str) -> CharResult {
    Err("unicode escape sequences are not implemented.")
}

fn read_big_u_escape_seq(_s: &str) -> CharResult {
    Err("unicode escape sequences are not implemented.")
}

fn read_big_n_escape_seq(_s: &str) -> CharResult {
    Err("name escape sequences are not implemented.")
}

fn read_escape_seq(s: &str) -> CharResult {
    Ok(match s.chars().next().ok_or("Unable to read character")? {
        'a' => ('\x07', &s[1..]),
        'b' => ('\x08', &s[1..]),
        'f' => ('\x0c', &s[1..]),
        'n' => ('\x0a', &s[1..]),
        'r' => ('\x0d', &s[1..]),
        't' => ('\x09', &s[1..]),
        'v' => ('\x0b', &s[1..]),
        '0'..='9' => read_onum_escape_seq(s)?,
        'o' => read_o_escape_seq(&s[1..])?,
        'x' => read_x_escape_seq(&s[1..])?,
        'u' => read_u_escape_seq(&s[1..])?,
        'U' => read_big_u_escape_seq(&s[1..])?,
        'N' => read_big_n_escape_seq(&s[1..])?,
        c => (c, &s[1..]),
    })
}

pub fn read_one_char_or_escape_sequence(s: &str) -> CharResult {
    Ok(match s.chars().next().ok_or("Unable to read character")? {
        '\\' => read_escape_seq(&s[1..])?,
        c => (c, &s[1..]),
    })
}

pub fn read(s: String) -> Result<(CharType, char), &'static str> {
    let (ty, s) = read_type_prefix(&s);
    let s = s
        .strip_prefix('\'')
        .ok_or("Missing opening '")?
        .strip_suffix('\'')
        .ok_or("Missing closing '")?;
    let (c, _) = read_one_char_or_escape_sequence(s)?;
    Ok((ty, c))
}
