use crate::{
    parsing::{context::Context, Token},
    util::loc::Location,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CVQualifiers {
    None,
    Const,
    Volatile,
    ConstVolatile,
}

impl CVQualifiers {
    pub fn maybe_update(&mut self, tok: &Token) -> bool {
        if tok.tok.is_kw("const") {
            if let Self::None = self {
                *self = Self::Const;
            } else if let Self::Volatile = self {
                *self = Self::ConstVolatile;
            } else {
                // TODO: Warn extra const
            }
            true
        } else if tok.tok.is_kw("volatile") {
            if let Self::None = self {
                *self = Self::Volatile;
            } else if let Self::Const = self {
                *self = Self::ConstVolatile;
            } else {
                // TODO: Warn extra volatile
            }
            true
        } else {
            false
        }
    }
    fn ty_str(&self) -> &'static str {
        match &self {
            Self::None => "",
            Self::Const => " const",
            Self::Volatile => " volatile",
            Self::ConstVolatile => " const volatile",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeType {
    Normal(String),
    Pointer(Box<Type>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub cv: CVQualifiers,
    pub ty: TypeType,
    pub loc: Location,
}

impl Type {
    fn read_type_name(tok: &Token) -> Option<String> {
        if tok.tok.is_kw("void") {
            return Some(String::from("void"));
        }
        if tok.tok.is_kw("int") {
            return Some(String::from("int"));
        }
        if tok.tok.is_kw("char") {
            return Some(String::from("char"));
        }
        tok.tok.ident()
    }

    pub fn parse<'a>(toks: &'a [Token], _: &mut Context) -> Option<(Self, &'a [Token])> {
        let loc_start = &toks.first()?.loc;
        let mut loc_end = &toks.first()?.loc;
        let mut cv = CVQualifiers::None;
        let mut toks = toks;
        let mut ty = None;
        while !toks.is_empty() {
            let fst = toks.first()?;
            if cv.maybe_update(fst) {
                toks = &toks[1..];
                loc_end = &fst.loc;
                continue;
            }
            if fst.tok.is_punct("*") {
                let ty2 = Self {
                    ty: ty.expect("Invalid type"),
                    cv,
                    loc: Location::from_merged(loc_start, loc_end),
                };
                loc_end = &fst.loc;
                ty = Some(TypeType::Pointer(Box::new(ty2)));
                cv = CVQualifiers::None;
                toks = &toks[1..];
                continue;
            }
            if ty.is_some() {
                break;
            }
            let name = Self::read_type_name(fst)?;
            ty = Some(TypeType::Normal(name));
            loc_end = &fst.loc;
            toks = &toks[1..];
        }
        let ty = ty?;
        Some((
            Self {
                cv,
                ty,
                loc: Location::from_merged(loc_start, loc_end),
            },
            toks,
        ))
    }

    pub fn ty_str(&self) -> String {
        let mut out = String::new();
        match &self.ty {
            TypeType::Normal(n) => {
                out.push_str(n);
                out.push_str(self.cv.ty_str());
            }
            TypeType::Pointer(n) => {
                out.push_str(&n.ty_str());
                out.push_str(" *");
                out.push_str(self.cv.ty_str());
            }
        }
        out
    }
}
