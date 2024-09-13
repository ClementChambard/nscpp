#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum IntegerType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    Usize,
    Isize,
}

fn should_be_bigger(n: usize, sign: bool) -> bool {
    if sign {
        n > 0x7fffffff
    } else {
        n > 0xffffffff
    }
}

fn get_signed_type(ty: IntegerType, sign: bool) -> IntegerType {
    match (sign, ty) {
        (true, IntegerType::U8) => IntegerType::I8,
        (true, IntegerType::I8) => IntegerType::I8,
        (true, IntegerType::U16) => IntegerType::I16,
        (true, IntegerType::I16) => IntegerType::I16,
        (true, IntegerType::U32) => IntegerType::I32,
        (true, IntegerType::I32) => IntegerType::I32,
        (true, IntegerType::U64) => IntegerType::I64,
        (true, IntegerType::I64) => IntegerType::I64,
        (true, IntegerType::Usize) => IntegerType::Isize,
        (true, IntegerType::Isize) => IntegerType::Isize,
        (false, IntegerType::U8) => IntegerType::U8,
        (false, IntegerType::I8) => IntegerType::U8,
        (false, IntegerType::U16) => IntegerType::U16,
        (false, IntegerType::I16) => IntegerType::U16,
        (false, IntegerType::U32) => IntegerType::U32,
        (false, IntegerType::I32) => IntegerType::U32,
        (false, IntegerType::U64) => IntegerType::U64,
        (false, IntegerType::I64) => IntegerType::U64,
        (false, IntegerType::Usize) => IntegerType::Usize,
        (false, IntegerType::Isize) => IntegerType::Usize,
    }
}

fn get_integer_type(n: usize, suffix: &str) -> Result<IntegerType, String> {
    let mut sign = true;
    let mut ty = IntegerType::I32;
    for c in suffix.chars() {
        if c == 'u' && sign {
            sign = false;
        } else if c == 'l' && ty == IntegerType::I32 {
            ty = IntegerType::I64;
        } else if c == 'z' && ty == IntegerType::I32 {
            ty = IntegerType::Isize;
        } else {
            return Err(format!("Invalid suffix '{}'", suffix));
        }
    }
    if ty == IntegerType::I32 && should_be_bigger(n, sign) {
        ty = IntegerType::I64;
    }

    Ok(get_signed_type(ty, sign))
}

pub fn read_digit(s: &str, base: usize) -> Option<usize> {
    let n = match s.chars().next()? {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
        'a' => 10,
        'b' => 11,
        'c' => 12,
        'd' => 13,
        'e' => 14,
        'f' => 15,
        _ => return None,
    };
    if n >= base {
        None
    } else {
        Some(n)
    }
}

pub fn parse_with_base(s: &str, base: usize) -> (usize, &str) {
    let mut s = s;
    let mut n = 0;
    while let Some(d) = read_digit(s, base) {
        n *= base;
        n += d;
        s = &s[1..];
    }
    (n, s)
}

pub fn read_with_base(s: &str, base: usize) -> Result<(IntegerType, usize), String> {
    let (n, suffix) = parse_with_base(s, base);
    Ok((get_integer_type(n, suffix)?, n))
}
