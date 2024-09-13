use crate::parsing::{
    ast::{decl::block_decl::TypeSpecifier, parse_util::parse_non_empty_elt_list},
    context::Context,
    Token,
};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum EnumKey {
    Enum,
    EnumClass,
    EnumStruct,
}

impl EnumKey {
    pub fn parse<'a>(toks: &'a [Token], _ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        toks.first()?.check_kw("enum")?;
        if let Some(fst) = toks.first() {
            if fst.tok.is_kw("class") {
                Some((Self::EnumClass, &toks[2..]))
            } else if fst.tok.is_kw("struct") {
                Some((Self::EnumStruct, &toks[2..]))
            } else {
                Some((Self::Enum, &toks[1..]))
            }
        } else {
            Some((Self::Enum, &toks[1..]))
        }
    }
}

// EnumBase ::= ':' NonEmptyList<TypeSpecifier>(Integer)
#[derive(Clone, PartialEq, Debug)]
pub struct EnumBase {
    spec: Vec<TypeSpecifier>,
}

impl EnumBase {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        toks.first()?.check_punct(":");
        let (spec, toks) = parse_non_empty_elt_list(toks, ctx, TypeSpecifier::parse)?;
        // TODO: Check if spec forms an integer type
        Some((Self { spec }, toks))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct EnumSpecifier {}

impl EnumSpecifier {
    pub fn parse<'a>(_toks: &'a [Token], _ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        None
    }
}
