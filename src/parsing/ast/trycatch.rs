use crate::{
    parsing::{
        ast::{
            attr::Attr,
            decl::block_decl::{Declarator, TypeSpecifier},
            parse_util::{parse_non_empty_elt_list, parse_opt},
            Stmt,
        },
        context::Context,
        Token,
    },
    util::{ast_printing, color, loc::Location},
};

#[derive(Clone, PartialEq, Debug)]
pub enum HandlerType {
    All,
    Some {
        attr: Option<Attr>,
        ty: Vec<TypeSpecifier>,
        decl: Declarator,
    },
}

impl HandlerType {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let (attr, toks) = parse_opt(toks, ctx, Attr::parse);
        let (ty, toks) = parse_non_empty_elt_list(toks, ctx, TypeSpecifier::parse)?;
        // TODO: Could be Opt<AbstractDeclarator>
        let (decl, toks) = Declarator::parse(toks, ctx)?;
        Some((Self::Some { attr, ty, decl }, toks))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Handler {
    pub ty: HandlerType,
    pub loc: Location,
    pub stmt: Stmt,
}

impl Handler {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        fst.check_kw("catch")?;
        toks[1..].first()?.check_punct("(")?;
        if toks[2..].first()?.tok.is_punct("...") {
            toks[3..].first()?.check_punct(")")?;
            let (stmt, toks) = Stmt::parse(&toks[4..], ctx)?;
            // TODO: should be compound ?
            return Some((
                Self {
                    ty: HandlerType::All,
                    loc: Location::from_merged(&fst.loc, &stmt.loc),
                    stmt,
                },
                toks,
            ));
        }
        let (ty, toks) = HandlerType::parse(&toks[2..], ctx)?;
        toks.first()?.check_punct(")")?;
        let (stmt, toks) = Stmt::parse(&toks[1..], ctx)?;
        Some((
            Self {
                ty,
                loc: Location::from_merged(&fst.loc, &stmt.loc),
                stmt,
            },
            toks,
        ))
    }
}

impl ast_printing::AstPrint for Handler {
    fn print_ast(&self, fstr: &str, lstr: &str) -> String {
        let mut out = ast_printing::start_str_with_loc(color::PURPLE, "CatchHandler", fstr, &self.loc);
        out.push_str(&format!(" {:?}\n", self.ty));
        out.push_str(&self.stmt.print_ast_end(lstr));
        out
    }
}
