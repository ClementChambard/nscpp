mod litterals;
mod punct;

use super::loc::Location;
use crate::file_opening::{PPTokType, PPToken};
pub use litterals::{merge_str_lit, CharType, LitType};
pub use punct::PunctType;

#[derive(Clone, PartialEq, Debug)]
pub enum TokType {
    Id(String),
    KW(String),
    Lit(LitType),
    Punct(PunctType),
}

impl TokType {
    fn from_ppident(id: String) -> Self {
        match &id[..] {
            "alignas" | "alignof" | "asm" | "atomic_cancel" | "atomic_commit"
            | "atomic_noexcept" | "auto" | "bool" | "break" | "case" | "catch" | "char"
            | "char8_t" | "char16_t" | "char32_t" | "class" | "concept" | "const" | "consteval"
            | "constexpr" | "constinit" | "const_cast" | "continue" | "co_await" | "co_return"
            | "co_yield" | "decltype" | "default" | "delete" | "do" | "double" | "dynamic_cast"
            | "else" | "enum" | "explicit" | "export" | "extern" | "float" | "for" | "friend"
            | "goto" | "if" | "inline" | "int" | "long" | "mutable" | "namespace" | "new"
            | "noexcept" | "operator" | "private" | "protected" | "public" | "reflexpr"
            | "register" | "reinterpret_cast" | "requires" | "return" | "short" | "signed"
            | "sizeof" | "static" | "static_assert" | "static_cast" | "struct" | "switch"
            | "synchronized" | "template" | "this" | "thread_local" | "throw" | "try"
            | "typedef" | "typeid" | "typename" | "union" | "unsigned" | "using" | "virtual"
            | "void" | "volatile" | "wchar_t" | "while" => Self::KW(id),
            "and" => Self::Punct(PunctType::AndAnd),
            "and_eq" => Self::Punct(PunctType::AndEq),
            "bitand" => Self::Punct(PunctType::And),
            "bitor" => Self::Punct(PunctType::Or),
            "compl" => Self::Punct(PunctType::Compl),
            "not" => Self::Punct(PunctType::Not),
            "not_eq" => Self::Punct(PunctType::NotEq),
            "or" => Self::Punct(PunctType::OrOr),
            "or_eq" => Self::Punct(PunctType::OrEq),
            "xor" => Self::Punct(PunctType::Xor),
            "xor_eq" => Self::Punct(PunctType::XorEq),
            "false" => Self::Lit(LitType::Bool(false)),
            "true" => Self::Lit(LitType::Bool(true)),
            "nullptr" => Self::Lit(LitType::Nullptr),
            _ => Self::Id(id),
        }
    }

    fn from_pptoktype(pptoktype: PPTokType) -> Result<Option<Self>, String> {
        Ok(Some(match pptoktype {
            PPTokType::Ident(s) => Self::from_ppident(s),
            PPTokType::NewLine | PPTokType::Whitespace => return Ok(None),
            PPTokType::Num(n) => Self::Lit(LitType::read_num(n)?),
            PPTokType::Chr(c) => Self::Lit(LitType::read_char(c)?),
            PPTokType::Str(s) => Self::Lit(LitType::read_str(s)?),
            PPTokType::Punct(p) => Self::Punct(PunctType::read(p)),
            _ => {
                return Err(String::from(
                    "Wrong preprocessing token found in token list",
                ))
            }
        }))
    }

    pub fn is_punct(&self, s: &str) -> bool {
        if let Self::Punct(p) = self {
            PunctType::read(String::from(s)) == *p
        } else {
            false
        }
    }

    pub fn is_kw(&self, s: &str) -> bool {
        if let Self::KW(k) = self {
            k == s
        } else {
            false
        }
    }

    pub fn is_ident(&self, s: &str) -> bool {
        if let Self::Id(i) = self {
            i == s
        } else {
            false
        }
    }

    pub fn ident(&self) -> Option<String> {
        if let Self::Id(s) = self {
            Some(s.clone())
        } else {
            None
        }
    }

    pub fn str_lit(&self) -> Option<String> {
        if let Self::Lit(LitType::String(s, _)) = self {
            Some(s.clone())
        } else {
            None
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Token {
    pub tok: TokType,
    pub loc: Location,
}

impl Token {
    pub fn from_pptoken(pptok: PPToken) -> Result<Option<Self>, String> {
        let loc = pptok.loc;
        let tok = TokType::from_pptoktype(pptok.tok)
            .map_err(|e| crate::ErrorBuilder::new().error_at_loc(&e, &loc).build())?;

        Ok(tok.map(|tok| Self { tok, loc }))
    }

    pub fn check_kw(&self, kw: &str) -> Option<()> {
        if self.tok.is_kw(kw) {
            Some(())
        } else {
            None
        }
    }

    pub fn check_punct(&self, p: &str) -> Option<()> {
        if self.tok.is_punct(p) {
            Some(())
        } else {
            None
        }
    }

    pub fn get_str(&self) -> String {
        match &self.tok {
            TokType::Id(i) => i.clone(),
            TokType::Punct(p) => String::from(p.get_str()),
            TokType::KW(k) => k.clone(),
            TokType::Lit(l) => l.get_str(),
        }
    }
}
