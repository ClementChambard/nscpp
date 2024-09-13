// functions for qualified / unqualified idents
// also template idents

use crate::{
    parsing::{
        context::{Context, NamespaceName},
        TokType, Token,
    },
    util::loc::Location,
};

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MaybeQualifiedIdent {
    from_global: bool,
    namespace: NamespaceName,
    name: String,
    loc: Location,
}

impl MaybeQualifiedIdent {
    pub fn parse_qualified<'a>(
        from_global: bool,
        toks: &'a [Token],
        _ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let fst_loc = &toks.first()?.loc;
        let last_loc;
        let mut toks = toks;
        let mut namespace = NamespaceName::new();
        let name;
        if from_global {
            toks = &toks[1..];
        } else {
            namespace.push(toks.first()?.tok.ident()?);
            toks = &toks[2..];
        }
        loop {
            let t = toks.first()?;
            let i = t.tok.ident()?;
            toks = &toks[1..];
            if let Some(t) = toks.first() {
                if !t.tok.is_punct("::") {
                    last_loc = &t.loc;
                    name = i;
                    break;
                }
                namespace.push(i);
                toks = &toks[1..];
            } else {
                last_loc = &t.loc;
                name = i;
                break;
            }
        }
        Some((
            Self {
                from_global,
                namespace,
                name,
                loc: Location::from_merged(fst_loc, last_loc),
            },
            toks,
        ))
    }

    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        if fst.tok.is_punct("::") {
            Self::parse_qualified(true, toks, ctx)
        } else if let TokType::Id(i) = &fst.tok {
            let fst_loc = &fst.loc;
            if toks.len() == 1 || !toks[1..].first()?.tok.is_punct("::") {
                Some((
                    Self {
                        from_global: false,
                        namespace: NamespaceName::new(),
                        name: i.clone(),
                        loc: fst_loc.clone(),
                    },
                    &toks[1..],
                ))
            } else {
                Self::parse_qualified(false, toks, ctx)
            }
        } else {
            None
        }
    }

    pub fn get_str(&self) -> String {
        let mut out = String::new();
        if self.from_global {
            out.push_str("::");
        }
        for s in &self.namespace {
            out.push_str(&format!("{s}::"));
        }
        out.push_str(&self.name);
        out
    }
}
