use crate::{
    parsing::{
        ast::parse_util::{
            balanced_tok_seq, parse_sep_comma, parse_sep_elt_list_nosep,
        },
        context::Context,
        Token,
    },
    util::{ast_printing, color, loc::Location},
};

#[derive(Clone, PartialEq, Debug)]
pub struct Attribute {
    pub namespace: Option<String>,
    pub name: String,
    pub tokens: Vec<Token>,
}

impl Attribute {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let mut name = toks.first()?.tok.ident()?;
        let mut toks = &toks[1..];
        let next = toks.first();
        let mut namespace = None;
        let mut tokens = Vec::new();
        if next.is_none() {
            return Some((
                Self {
                    namespace,
                    name,
                    tokens,
                },
                toks,
            ));
        }
        let mut next = next.unwrap();
        if next.tok.is_punct("::") {
            namespace = Some(name);
            name = toks[1..].first()?.tok.ident()?;
            toks = &toks[2..];
            let n = toks.first();
            if n.is_none() {
                return Some((
                    Self {
                        namespace,
                        name,
                        tokens,
                    },
                    toks,
                ));
            }
            next = n.unwrap();
        }
        if !next.tok.is_punct("(") {
            return Some((
                Self {
                    namespace,
                    name,
                    tokens,
                },
                toks,
            ));
        }
        (tokens, toks) = balanced_tok_seq(&toks[1..], ctx)?;
        toks.first()?.check_punct(")")?;
        Some((
            Self {
                namespace,
                name,
                tokens,
            },
            &toks[1..],
        ))
    }

    pub fn ast_str(&self) -> String {
        let mut out = String::new();
        if let Some(namespace) = &self.namespace {
            out.push_str(namespace);
            out.push_str("::");
        }
        out.push_str(&self.name);
        if !self.tokens.is_empty() {
            out.push('(');
            out.push_str(&self.tokens.first().unwrap().get_str());
            for t in &self.tokens[1..] {
                out.push(' ');
                out.push_str(&t.get_str());
            }
            out.push(')');
        }
        out
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Attr {
    pub namespace: Option<String>,
    pub attrs: Vec<Attribute>,
    pub loc: Location,
}

impl Attr {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let first = toks.first()?;
        first.check_punct("[")?;
        toks[1..].first()?.check_punct("[")?;
        let mut toks = &toks[2..];
        let mut namespace = None;
        if toks.first()?.tok.is_kw("using") {
            namespace = Some(toks[1..].first()?.tok.ident()?);
            toks[2..].first()?.check_punct(":")?;
            toks = &toks[3..];
        }
        let (attrs, toks) = parse_sep_elt_list_nosep(toks, ctx, Attribute::parse, parse_sep_comma)?;
        // TODO: if has namespace, child can't have namespace
        toks.first()?.check_punct("]")?;
        let last = toks[1..].first()?;
        last.check_punct("]")?;
        Some((
            Self {
                namespace,
                attrs,
                loc: Location::from_merged(&first.loc, &last.loc),
            },
            &toks[2..],
        ))
    }
}

impl ast_printing::AstPrint for Attr {
    fn print_ast(&self, fstr: &str, _lstr: &str) -> String {
        let mut out = ast_printing::start_str_with_loc(color::BLUE, "Attribute", fstr, &self.loc);
        if let Some(namespace) = &self.namespace {
            out.push_str(&color::format_str().s_cb(&format!(" using {namespace}"), color::CYAN).build());
        }
        let mut builder = color::format_str().col(color::PURPLE).s(" [");
        for (i, a) in self.attrs.iter().enumerate() {
            if i != 0 {
                builder = builder.s(", ");
            }
            builder = builder.s(&a.ast_str());
        }
        out.push_str(&builder.c(']').build());
        out
    }
}
