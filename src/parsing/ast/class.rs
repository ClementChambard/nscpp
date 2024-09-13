use crate::parsing::{
    ast::{
        attr::Attr,
        ident::MaybeQualifiedIdent,
        parse_util::{
            parse_elt_list, parse_non_empty_sep_elt_list, parse_opt, parse_sep_comma,
            sep_list_remove_sep,
        },
    },
    context::Context,
    Token,
};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ClassKey {
    Class,
    Struct,
    Union,
}

impl ClassKey {
    pub fn parse<'a>(toks: &'a [Token], _ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        Some((
            if fst.tok.is_kw("class") {
                Self::Class
            } else if fst.tok.is_kw("struct") {
                Self::Struct
            } else if fst.tok.is_kw("union") {
                Self::Union
            } else {
                return None;
            },
            &toks[1..],
        ))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MemberSpec {}

impl MemberSpec {
    pub fn parse<'a>(_toks: &'a [Token], _ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        None
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum AccessSpecifier {
    Public,
    Protected,
    Private,
}

impl AccessSpecifier {
    pub fn parse<'a>(toks: &'a [Token], _ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        if fst.tok.is_kw("public") {
            Some((Self::Public, &toks[1..]))
        } else if fst.tok.is_kw("private") {
            Some((Self::Private, &toks[1..]))
        } else if fst.tok.is_kw("protected") {
            Some((Self::Protected, &toks[1..]))
        } else {
            None
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct BaseSpec {
    attr: Option<Attr>,
    access: Option<AccessSpecifier>,
    is_virtual: bool,
    name: MaybeQualifiedIdent,
}

impl BaseSpec {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let (attr, mut toks) = parse_opt(toks, ctx, Attr::parse);
        let fst = toks.first()?;
        let mut is_virtual = false;
        let mut access = None;
        if fst.tok.is_kw("virtual") {
            is_virtual = true;
            toks = &toks[1..];
        } else if fst.tok.is_kw("public") {
            access = Some(AccessSpecifier::Public);
            toks = &toks[1..];
        } else if fst.tok.is_kw("private") {
            access = Some(AccessSpecifier::Private);
            toks = &toks[1..];
        } else if fst.tok.is_kw("protected") {
            access = Some(AccessSpecifier::Protected);
            toks = &toks[1..];
        }
        let fst = toks.first()?;
        if access.is_some() && fst.tok.is_kw("virtual") {
            is_virtual = true;
            toks = &toks[1..];
        } else if is_virtual && fst.tok.is_kw("public") {
            access = Some(AccessSpecifier::Public);
            toks = &toks[1..];
        } else if is_virtual && fst.tok.is_kw("protected") {
            access = Some(AccessSpecifier::Protected);
            toks = &toks[1..];
        } else if is_virtual && fst.tok.is_kw("private") {
            access = Some(AccessSpecifier::Private);
            toks = &toks[1..];
        }
        let (name, toks) = MaybeQualifiedIdent::parse(toks, ctx)?;
        // TODO: actually, it is a ClassOrComputed
        // ClassOrComputed ::= Opt<NestedNameSpecifier> id(TypeName)
        //                   | NestedNameSpecifier 'template' SimpleTemplateId??
        //                   | DecltypeSpecifier
        //                   | PackIndexingSpecifier
        //                   ;
        Some((
            Self {
                attr,
                access,
                is_virtual,
                name,
            },
            toks,
        ))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassSpecifier {
    key: ClassKey,
    attr: Option<Attr>,
    name: Option<MaybeQualifiedIdent>,
    is_final: bool,
    base: Vec<BaseSpec>,
    members: Vec<MemberSpec>,
}

impl ClassSpecifier {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let (key, toks) = ClassKey::parse(toks, ctx)?;
        let (attr, toks) = parse_opt(toks, ctx, Attr::parse);
        let (name, mut toks) = parse_opt(toks, ctx, MaybeQualifiedIdent::parse);
        let is_final = toks.first()?.tok.is_ident("final");
        if is_final {
            if name.is_none() {
                panic!("Error final class with no name");
            }
            toks = &toks[1..];
        }
        let fst = toks.first()?;
        let base = if fst.tok.is_punct(":") {
            let (base, t) =
                parse_non_empty_sep_elt_list(&toks[1..], ctx, BaseSpec::parse, parse_sep_comma)?;
            toks = t;
            sep_list_remove_sep(base)
        } else {
            Vec::new()
        };
        toks.first()?.check_punct("{")?;
        let (members, toks) = parse_elt_list(&toks[1..], ctx, MemberSpec::parse)?;
        toks.first()?.check_punct("}")?;
        Some((
            Self {
                key,
                attr,
                name,
                is_final,
                base,
                members,
            },
            &toks[1..],
        ))
    }
}
