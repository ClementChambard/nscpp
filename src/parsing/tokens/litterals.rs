mod char;
mod float;
mod integer;
mod string;

pub use char::CharType;
pub use float::FloatType;
pub use integer::IntegerType;
pub use string::merge_str_lit;

#[derive(Clone, PartialEq, Debug)]
pub enum LitType {
    Int(usize, IntegerType),
    Float(f64, FloatType),
    Char(char, CharType),
    String(String, CharType),
    Bool(bool),
    Nullptr,
}

impl LitType {
    pub fn read_num(n: String) -> Result<Self, String> {
        let n = n.to_lowercase();
        Ok(if let Some(n) = n.strip_prefix("0b") {
            let (ty, n) = integer::read_with_base(n, 2)
                .map_err(|e| format!("Error reading binary integer litteral: {}", e))?;
            Self::Int(n, ty)
        } else if let Some(n) = n.strip_prefix("0x") {
            if n.contains('p') || n.contains('.') {
                let (ty, n) = float::read_hex(n)
                    .map_err(|e| format!("Error reading hex float litteral: {}", e))?;
                Self::Float(n, ty)
            } else {
                let (ty, n) = integer::read_with_base(&n[2..], 16)
                    .map_err(|e| format!("Error reading hex integer litteral: {}", e))?;
                Self::Int(n, ty)
            }
        } else if n.contains('.') || n.contains('e') {
            let (ty, n) =
                float::read(&n).map_err(|e| format!("Error reading float litteral: {}", e))?;
            Self::Float(n, ty)
        } else if let Some(n) = n.strip_prefix('0') {
            let (ty, n) = integer::read_with_base(n, 8)
                .map_err(|e| format!("Error reading octal integer litteral: {}", e))?;
            Self::Int(n, ty)
        } else {
            let (ty, n) = integer::read_with_base(&n, 10)
                .map_err(|e| format!("Error reading integer litteral: {}", e))?;
            Self::Int(n, ty)
        })
    }
    pub fn read_char(c: String) -> Result<Self, String> {
        let (ty, c) = char::read(c).map_err(|e| format!("Error reading char litteral: {}", e))?;
        Ok(Self::Char(c, ty))
    }
    pub fn read_str(s: String) -> Result<Self, String> {
        let (ty, s) =
            string::read(s).map_err(|e| format!("Error reading string litteral: {}", e))?;
        Ok(Self::String(s, ty))
    }

    pub fn get_str(&self) -> String {
        match &self {
            Self::Int(i, _) => format!("{i:?}"),
            Self::Float(f, _) => format!("{f:?}"),
            Self::Char(c, t) => format!("{}{c:?}", t.get_str()),
            Self::String(s, t) => format!("{}{s:?}", t.get_str()),
            Self::Bool(b) => format!("{b:?}"),
            Self::Nullptr => String::from("nullptr"),
        }
    }
}
