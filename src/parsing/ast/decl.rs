pub mod block_decl;

use crate::{
    parsing::{
        ast::{
            attr::Attr,
            decl::block_decl::BlockDeclaration,
            parse_util::{
                parse_elt_list, parse_sep_comma, parse_sep_elt_list, sep_list_remove_sep,
            },
            Stmt, Type,
        },
        context::Context,
        TokType, Token,
    },
    util::{ast_printing, color, loc::Location},
};

// Decl ::= FunctionDecl
//        | TemplateDecl // partial template specialization
//        | ExplicitTemplateInstanciation
//        | ExplicitTemplateSpecialization
//        | NamespaceDefinition
//        | LinkageSpecification
//        | Opt<Attr> ';'
//        | Opt<Attr> Declarator(function) ';' (function declaration without a decl-specifier-seq)
//        | BlockDeclaration
//        ;


#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ParamDecl {
    pub ty: Type,
    pub name: Option<String>,
    pub loc: Location,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Noexcept {}

impl Noexcept {
    pub fn parse<'a>(toks: &'a [Token], _ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        if toks.first()?.tok.is_kw("noexcept") {
            // TODO: noexcept(ConstExpr) should evaluate expression
            Some((Self {}, &toks[1..]))
        } else {
            None
        }
    }
}

impl ParamDecl {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let (ty, mut toks) = Type::parse(toks, ctx)?;
        let mut name = None;
        let mut loc = ty.loc.clone();
        if let Some(t) = toks.first() {
            if let TokType::Id(id) = &t.tok {
                toks = &toks[1..];
                loc = loc.merge(t.loc.clone());
                name = Some(id.clone());
            }
        }
        Some((Self { loc, ty, name }, toks))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum DeclType {
    Function {
        name: String,
        ret: Type,
        params: Vec<ParamDecl>,
        body: Option<Stmt>,
    },
    // TemplateDecl
    // ExplicitTemplateInstanciation
    // ExplicitTemplateSpecialization
    Namespace(String, Vec<Decl>),
    Linkage(String, Vec<Decl>),
    Empty(Option<Attr>),
    // Opt<Attr> Declarator(function) ';'
    Block(BlockDeclaration),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Decl {
    decl: DeclType,
    loc: Location,
}

impl Decl {
    fn parse_linkage<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let loc_start = &toks.first()?.loc;
        let name = toks[1..].first()?.tok.str_lit()?;
        let toks = &toks[2..];
        if toks.first()?.tok.is_punct("{") {
            let (decls, toks) = parse_elt_list(&toks[1..], ctx, Decl::parse)?;
            let last = toks.first()?;
            last.check_punct("}")?;
            Some((
                Self {
                    loc: Location::from_merged(loc_start, &last.loc),
                    decl: DeclType::Linkage(name, decls),
                },
                &toks[1..],
            ))
        } else {
            let (decl, toks) = Decl::parse(toks, ctx)?;
            Some((
                Self {
                    loc: Location::from_merged(loc_start, &decl.loc),
                    decl: DeclType::Linkage(name, vec![decl]),
                },
                toks,
            ))
        }
    }

    fn parse_namespace<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let loc_start = &toks.first()?.loc;
        let toks = &toks[1..];
        let name = toks.first()?.tok.ident()?;
        let toks = &toks[1..];
        if !toks.first()?.tok.is_punct("{") {
            return None;
        }
        let (decls, toks) = parse_elt_list(&toks[1..], ctx, Decl::parse)?;
        let close = toks.first()?;
        if !close.tok.is_punct("}") {
            return None;
        }
        Some((
            Self {
                loc: Location::from_merged(loc_start, &close.loc),
                decl: DeclType::Namespace(name, decls),
            },
            &toks[1..],
        ))
    }

    fn parse_function<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let (ret, toks) = Type::parse(toks, ctx)?;
        let name = toks.first()?.tok.ident()?;
        let toks = &toks[1..];
        if !toks.first()?.tok.is_punct("(") {
            return None;
        }
        let (params, toks) =
            parse_sep_elt_list(&toks[1..], ctx, ParamDecl::parse, parse_sep_comma)?;
        let params = sep_list_remove_sep(params);
        if !toks.first()?.tok.is_punct(")") {
            return None;
        }
        let toks = &toks[1..];
        let next = toks.first()?;
        if next.tok.is_punct(";") {
            Some((
                Self {
                    loc: Location::from_merged(&ret.loc, &next.loc),
                    decl: DeclType::Function {
                        name,
                        ret,
                        params,
                        body: None,
                    },
                },
                &toks[1..],
            ))
        } else if next.tok.is_punct("{") {
            let (compound, toks) = Stmt::parse(toks, ctx)?;
            Some((
                Self {
                    loc: Location::from_merged(&ret.loc, &compound.loc),
                    decl: DeclType::Function {
                        name,
                        ret,
                        params,
                        body: Some(compound),
                    },
                },
                toks,
            ))
        } else {
            None
        }
    }

    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        if fst.tok.is_kw("namespace") {
            Self::parse_namespace(toks, ctx)
        } else if fst.tok.is_kw("extern") {
            Self::parse_linkage(toks, ctx)
        } else if let Some(res) = Self::parse_function(toks, ctx) {
            Some(res)
        } else if let Some((attr, toks)) = Attr::parse(toks, ctx) {
            fst.check_punct(";")?;
            Some((
                Self {
                    loc: Location::from_merged(&attr.loc, &fst.loc),
                    decl: DeclType::Empty(Some(attr)),
                },
                &toks[1..],
            ))
        } else if fst.tok.is_punct(";") {
            Some((
                Self {
                    loc: fst.loc.clone(),
                    decl: DeclType::Empty(None),
                },
                &toks[1..],
            ))
        } else {
            let (res, toks) = BlockDeclaration::parse(toks, ctx)?;
            Some((
                Self {
                    decl: DeclType::Block(res),
                    loc: Location::new("dummy", 0, 0),
                },
                toks,
            ))
        }
    }

    fn function_type_str(ret: &Type, params: &[ParamDecl]) -> String {
        let mut out = String::new();
        out.push_str(&ret.ty_str());
        out.push_str(" (");
        if params.is_empty() {
            out.push(')');
            return out;
        }
        for p in params.iter().take(params.len() - 1) {
            out.push_str(&p.ty.ty_str());
            out.push_str(", ");
        }
        out.push_str(&params.last().unwrap().ty.ty_str());
        out.push(')');
        out
    }
}


impl ast_printing::AstPrint for ParamDecl {
    fn print_ast(&self, fstr: &str, _: &str) -> String {
        let mut out = ast_printing::start_str_with_loc(color::GREEN, "ParamVarDecl", fstr, &self.loc);
        if let Some(n) = &self.name {
            out.push_str(&color::format_str().c(' ').s_cb(&n, color::CYAN).build())
        }
        out.push_str(&color::format_str().c(' ').s_c(&self.ty.ty_str(), color::GREEN).build());
        out
    }
}

impl ast_printing::AstPrint for Decl {
    fn print_ast(&self, fstr: &str, lstr: &str) -> String {
        match &self.decl {
            DeclType::Function {
                name: n,
                ret: r,
                params: p,
                body: b,
            } => {
                let mut out = ast_printing::start_str_with_loc(color::GREEN, "FunctionDecl", fstr, &self.loc);
                out.push_str(&color::format_str().c(' ').s_cb(n, color::CYAN)
                             .s_c(&format!(" '{}'\n", Self::function_type_str(r, p)), color::GREEN)
                             .build());
                if let Some(b) = b {
                    for p in p.iter() {
                        out.push_str(&p.print_ast_mid(lstr));
                        out.push('\n');
                    }
                    out.push_str(&b.print_ast_end(lstr));
                } else {
                    out.push_str(&ast_printing::print_ast_list(p, lstr));
                }
                out
            }
            DeclType::Linkage(l, d) => {
                let mut out = ast_printing::start_str_with_loc(color::GREEN, "LinkageSpecDecl", fstr, &self.loc);
                out.push_str(&format!(" {}", l));
                if !d.is_empty() { out.push('\n'); }
                out.push_str(&ast_printing::print_ast_list(d, lstr));
                out
            }
            DeclType::Namespace(n, d) => {
                let mut out = ast_printing::start_str_with_loc(color::GREEN, "NamespaceDecl", fstr, &self.loc);
                out.push_str(&color::format_str().c(' ').s_cb(&n, color::CYAN).build());
                if !d.is_empty() { out.push('\n'); }
                out.push_str(&ast_printing::print_ast_list(d, lstr));
                out
            }
            DeclType::Empty(a) => {
                let mut out = ast_printing::start_str_with_loc(color::GREEN, "EmptyDecl", fstr, &self.loc);
                if let Some(a) = a {
                    out.push('\n');
                    out.push_str(&a.print_ast_end(lstr));
                }
                out
            }
            DeclType::Block(b) => b.print_ast(fstr, lstr),
        }
    }
}
