#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FloatType {
    Float,
    Double,
    LongDouble,
    Float16,
    Float32,
    Float64,
    Float128,
    BFloat16,
}

use super::integer::{parse_with_base, read_digit};

fn get_float_type(suffix: &str) -> Result<FloatType, &'static str> {
    Ok(match suffix {
        "f" => FloatType::Float,
        "" => FloatType::Double,
        "l" => FloatType::LongDouble,
        "f16" => FloatType::Float16,
        "f32" => FloatType::Float32,
        "f64" => FloatType::Float64,
        "f128" => FloatType::Float128,
        "bf16" => FloatType::BFloat16,
        _ => return Err("Invalid float litteral suffix"),
    })
}

fn parse_decimal_with_base<'a>(
    before: &'a str,
    after: &'a str,
    base: usize,
) -> Result<(f64, &'a str), &'static str> {
    let (v, s) = parse_with_base(before, base);
    if !s.is_empty() {
        return Err("unexpected character before decimal point");
    }
    let mut val = v as f64;
    let mut div = base as f64;
    let mut deci_str = after;
    while let Some(d) = read_digit(deci_str, base) {
        val += d as f64 / div;
        div *= base as f64;
        deci_str = &deci_str[1..];
    }
    Ok((val, deci_str))
}

fn parse_exponent_with_base(
    before: &str,
    after: &str,
    base: usize,
    e_base: f64,
) -> Result<(FloatType, f64), &'static str> {
    let (val, suffix) = if before.contains('.') {
        let mut splits = before.split('.');
        let before = splits.next().ok_or("unexpected missing decimal point")?;
        let after = splits.next().ok_or("unexpected missing decimal point")?;
        if splits.next().is_some() {
            return Err("Too many decimal point");
        }
        parse_decimal_with_base(before, after, base)?
    } else {
        let (v, s) = parse_with_base(before, base);
        (v as f64, s)
    };
    if !suffix.is_empty() {
        return Err("unexpected character before exponent");
    }
    let mut exp_str = after;
    let sign = if exp_str.starts_with('+') {
        exp_str = &exp_str[1..];
        1i32
    } else if exp_str.starts_with('-') {
        exp_str = &exp_str[1..];
        -1i32
    } else {
        1i32
    };
    let (exp, suffix) = parse_with_base(exp_str, 10);
    Ok((
        get_float_type(suffix)?,
        val * e_base.powi(exp as i32 * sign),
    ))
}

pub fn read(s: &str) -> Result<(FloatType, f64), &'static str> {
    if s.contains('e') {
        let mut splits = s.split('e');
        let before = splits.next().ok_or("unexpected missing exponent symbol")?;
        let after = splits.next().ok_or("unexpected missing exponent symbol")?;
        if splits.next().is_some() {
            return Err("Too many exponent symbol");
        }
        return parse_exponent_with_base(before, after, 10, 10f64);
    }
    let mut splits = s.split('.');
    let before = splits.next().ok_or("Missing decimal point")?;
    let after = splits.next().ok_or("Missing decimal point")?;
    if splits.next().is_some() {
        return Err("Too many decimal point");
    }
    let (val, suffix) = parse_decimal_with_base(before, after, 10)?;
    Ok((get_float_type(suffix)?, val))
}

pub fn read_hex(s: &str) -> Result<(FloatType, f64), &'static str> {
    if s.contains('p') {
        let mut splits = s.split('p');
        let before = splits.next().ok_or("unexpected missing exponent symbol")?;
        let after = splits.next().ok_or("unexpected missing exponent symbol")?;
        if splits.next().is_some() {
            return Err("Too many exponent symbol");
        }
        return parse_exponent_with_base(before, after, 16, 2f64);
    }
    let mut splits = s.split('.');
    let before = splits.next().ok_or("Missing decimal point")?;
    let after = splits.next().ok_or("Missing decimal point")?;
    if splits.next().is_some() {
        return Err("Too many decimal point");
    }
    let (val, suffix) = parse_decimal_with_base(before, after, 16)?;
    Ok((get_float_type(suffix)?, val))
}
