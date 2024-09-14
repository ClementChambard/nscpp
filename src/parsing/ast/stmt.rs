use crate::{
    parsing::{
        ast::{
            attr::Attr,
            decl::block_decl::{BlockDeclaration, Initializer},
            parse_util::{self, parse_elt_list, parse_non_empty_elt_list, parse_opt},
            trycatch::Handler,
            Expr,
        },
        context::Context,
        Token,
    },
    util::{ast_printing, color, loc::Location},
};

#[derive(Clone, PartialEq, Debug)]
pub enum Label {
    // TODO:
    Normal(Option<Attr>, String), // ::= Opt<Attr> id ':'
    Case(Option<Attr>, Expr),     // ::= Opt<Attr> 'case' Expr(Constexpr) ':'
    Default(Option<Attr>),        // ::= Opt<Attr> 'default' ':'
}

impl Label {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let (attr, toks) = parse_opt(toks, ctx, Attr::parse);
        let fst = toks.first()?;
        if fst.tok.is_kw("default") {
            toks[1..].first()?.check_punct(":")?;
            Some((Self::Default(attr), &toks[2..]))
        } else if fst.tok.is_kw("case") {
            let (expr, toks) = Expr::parse(&toks[1..], ctx)?;
            toks.first()?.check_punct(":")?;
            Some((Self::Case(attr, expr), &toks[1..]))
        } else {
            let id = fst.tok.ident()?;
            toks[1..].first()?.check_punct(":")?;
            Some((Self::Normal(attr, id), &toks[2..]))
        }
    }

    pub fn ast_str(&self) -> String {
        match &self {
            Label::Normal(_, s) => s.clone(),
            Label::Case(_, _) => String::from("case"), // TODO: all
            Label::Default(_) => String::from("default"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum StmtType {
    Labeled(Label, Box<Stmt>),
    Expr(Expr),
    Null,
    Compound(Vec<Stmt>, Vec<Label>),
    If(bool, Option<Box<Stmt>>, Expr, Box<Stmt>, Option<Box<Stmt>>),
    IfConsteval(bool, Box<Stmt>, Option<Box<Stmt>>),
    Switch(Option<Box<Stmt>>, Expr, Box<Stmt>),
    While(Expr, Box<Stmt>),
    DoWhile(Expr, Box<Stmt>),
    For(Box<Stmt>, Option<Expr>, Option<Expr>, Box<Stmt>),
    // ForEach(Option<Box<Stmt>>, ForRangeDecl, ForRangeInit, Box<Stmt>),
    Break,
    Continue,
    Return(Option<Expr>),
    ReturnInit(Initializer),
    Goto(String),
    BlockDecl(BlockDeclaration),
    Try(Box<Stmt>, Vec<Handler>),
}

impl StmtType {
    fn name(&self) -> &'static str {
        match &self {
            Self::Labeled(_, _) => "LabeledStmt",
            Self::Expr(_) => "ExprStmt",
            Self::Null => "NullStmt",
            Self::Compound(_, _) => "CompoundStmt",
            Self::If(_, _, _, _, _) => "IfStmt",
            Self::IfConsteval(_, _, _) => "IfStmt",
            Self::Switch(_, _, _) => "SwitchStmt",
            Self::While(_, _) => "WhileStmt",
            Self::DoWhile(_, _) => "DoWhileStmt",
            Self::For(_, _, _, _) => "ForStmt",
            // Self::ForEach(Option<Box<Stmt>>, ForRangeDecl, ForRangeInit, Box<Stmt>),
            Self::Break => "BreakStmt",
            Self::Continue => "ContinueStmt",
            Self::Return(_) => "ReturnStmt",
            Self::ReturnInit(_) => "ReturnStmt",
            Self::Goto(_) => "GotoStmt",
            Self::BlockDecl(_) => "BlockDeclStmt",
            Self::Try(_, _) => "TryStmt",
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Stmt {
    pub stmt: StmtType,
    pub attr: Option<Attr>,
    pub loc: Location,
}

impl Stmt {
    fn parse_compound_with_attr<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        fst.check_punct("{")?;
        let toks = &toks[1..];
        let (ls, toks) = parse_elt_list(toks, ctx, Self::parse)?;
        let (labels, toks) = parse_elt_list(toks, ctx, Label::parse)?;
        let next = toks.first()?;
        if !next.tok.is_punct("}") {
            None
        } else {
            Some((
                Self {
                    loc: Location::from_merged(&fst.loc, &next.loc),
                    stmt: StmtType::Compound(ls, labels),
                    attr,
                },
                &toks[1..],
            ))
        }
    }

    pub fn parse_compound<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let (attr, toks) = parse_opt(toks, ctx, Attr::parse);
        Self::parse_compound_with_attr(attr, toks, ctx)
    }

    fn parse_expr<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let (expr, toks) = Expr::parse(toks, ctx)?;
        let next = toks.first()?;
        next.check_punct(";")?;
        let loc = if let Some(a) = &attr {
            Location::from_merged(&a.loc, &next.loc)
        } else {
            Location::from_merged(&expr.loc, &next.loc)
        };
        Some((
            Self {
                loc,
                stmt: StmtType::Expr(expr),
                attr,
            },
            &toks[1..],
        ))
    }

    fn parse_return<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let loc_start = if let Some(a) = &attr {
            &a.loc
        } else {
            &toks.first()?.loc
        };
        let (stmt, toks) = if toks[1..].first()?.tok.is_punct("{") {
            let (initializer, toks) = Initializer::parse_clause(&toks[1..], ctx)?;
            (StmtType::ReturnInit(initializer), toks)
        } else {
            let (expr, toks) = parse_opt(&toks[1..], ctx, Expr::parse);
            (StmtType::Return(expr), toks)
        };
        let semi = toks.first()?;
        semi.check_punct(";")?;
        Some((
            Self {
                loc: Location::from_merged(loc_start, &semi.loc),
                stmt,
                attr,
            },
            &toks[1..],
        ))
    }

    fn parse_if_consteval<'a>(
        attr: Option<Attr>,
        loc_start: &Location,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let loc_start = if let Some(a) = &attr {
            &a.loc
        } else {
            loc_start
        };
        let mut toks = toks;
        let not = if toks.first()?.tok.is_punct("!") {
            toks = &toks[1..];
            true
        } else {
            false
        };
        toks.first()?.check_kw("consteval")?;
        let (stmt1, stmt2, loc_end, toks) = Self::parse_if_end(&toks[1..], ctx)?;
        // TODO: Standard says stmt1 must be compound
        Some((
            Self {
                loc: Location::from_merged(loc_start, &loc_end),
                attr,
                stmt: StmtType::IfConsteval(not, Box::new(stmt1), stmt2.map(Box::new)),
            },
            toks,
        ))
    }

    fn parse_if_end<'a>(
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, Option<Self>, Location, &'a [Token])> {
        let (stmt1, mut toks) = Stmt::parse(toks, ctx)?;
        let mut loc_end = stmt1.loc.clone();
        let mut stmt2 = None;
        if let Some(else_tok) = toks.first() {
            if else_tok.tok.is_kw("else") {
                let (s2, o_toks) = Stmt::parse(&toks[1..], ctx)?;
                toks = o_toks;
                loc_end = s2.loc.clone();
                stmt2 = Some(s2);
            }
        }
        Some((stmt1, stmt2, loc_end, toks))
    }

    fn parse_if<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let mut toks = &toks[1..];
        let fst = toks.first()?;
        if fst.tok.is_kw("consteval") || fst.tok.is_punct("!") {
            return Self::parse_if_consteval(attr, &toks.first()?.loc, toks, ctx);
        }
        let loc_start = if let Some(a) = &attr {
            &a.loc
        } else {
            &toks.first()?.loc
        };
        let constexpr = if fst.tok.is_kw("constexpr") {
            toks = &toks[1..];
            true
        } else {
            false
        };
        let (init_stmt, toks) = parse_opt(toks, ctx, Stmt::parse);
        toks.first()?.check_punct("(")?;
        let (expr, toks) = Expr::parse(&toks[1..], ctx)?;
        toks.first()?.check_punct(")")?;
        let (stmt1, stmt2, loc_end, toks) = Self::parse_if_end(&toks[1..], ctx)?;
        Some((
            Self {
                loc: Location::from_merged(loc_start, &loc_end),
                stmt: StmtType::If(
                    constexpr,
                    init_stmt.map(Box::new),
                    expr,
                    Box::new(stmt1),
                    stmt2.map(Box::new),
                ),
                attr,
            },
            toks,
        ))
    }

    fn parse_switch<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let loc_start = if let Some(a) = &attr {
            &a.loc
        } else {
            &toks.first()?.loc
        };
        toks[1..].first()?.check_punct("(")?;
        let (init, toks) = parse_opt(&toks[2..], ctx, Stmt::parse);
        let (expr, toks) = Expr::parse(toks, ctx)?;
        toks.first()?.check_punct(")")?;
        let (stmt, toks) = Stmt::parse(&toks[1..], ctx)?;
        Some((
            Self {
                loc: Location::from_merged(loc_start, &stmt.loc),
                attr,
                stmt: StmtType::Switch(init.map(Box::new), expr, Box::new(stmt)),
            },
            toks,
        ))
    }

    fn parse_while<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let loc_start = if let Some(a) = &attr {
            &a.loc
        } else {
            &toks.first()?.loc
        };
        toks[1..].first()?.check_punct("(")?;
        let (expr, toks) = Expr::parse(&toks[2..], ctx)?;
        toks.first()?.check_punct(")")?;
        let (stmt, toks) = Stmt::parse(&toks[1..], ctx)?;
        Some((
            Self {
                loc: Location::from_merged(loc_start, &stmt.loc),
                attr,
                stmt: StmtType::While(expr, Box::new(stmt)),
            },
            toks,
        ))
    }

    fn parse_dowhile<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let loc_start = if let Some(a) = &attr {
            &a.loc
        } else {
            &toks.first()?.loc
        };
        let (stmt, toks) = Stmt::parse(&toks[1..], ctx)?;
        toks.first()?.check_kw("while")?;
        toks[1..].first()?.check_punct("(")?;
        let (expr, toks) = Expr::parse(&toks[2..], ctx)?;
        toks.first()?.check_punct(")")?;
        let last = toks[1..].first()?;
        last.check_punct(";")?;
        Some((
            Self {
                loc: Location::from_merged(loc_start, &last.loc),
                attr,
                stmt: StmtType::DoWhile(expr, Box::new(stmt)),
            },
            &toks[2..],
        ))
    }

    fn parse_for<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let loc_start = if let Some(a) = &attr {
            &a.loc
        } else {
            &toks.first()?.loc
        };
        toks[1..].first()?.check_punct("(")?;
        let (init, toks) = parse_opt(&toks[2..], ctx, Stmt::parse);
        // TODO: range based for loops

        // not a ranged based:
        // init is not optionnal:
        let init = init?;
        let (cond, toks) = parse_opt(toks, ctx, Expr::parse);
        toks.first()?.check_punct(";")?;
        let (post, toks) = parse_opt(&toks[1..], ctx, Expr::parse);
        toks.first()?.check_punct(")")?;
        let (stmt, toks) = Stmt::parse(&toks[1..], ctx)?;
        Some((
            Self {
                loc: Location::from_merged(loc_start, &stmt.loc),
                attr,
                stmt: StmtType::For(Box::new(init), cond, post, Box::new(stmt)),
            },
            toks,
        ))
    }

    fn parse_try<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        let loc_start = if let Some(a) = &attr {
            &a.loc
        } else {
            &toks.first()?.loc
        };
        let (stmt, toks) = Stmt::parse(&toks[1..], ctx)?;
        // TODO: should be compound
        let (handlers, toks) = parse_non_empty_elt_list(toks, ctx, Handler::parse)?;
        let last_loc = &handlers.last()?.loc;
        Some((
            Self {
                loc: Location::from_merged(loc_start, last_loc),
                attr,
                stmt: StmtType::Try(Box::new(stmt), handlers),
            },
            toks,
        ))
    }

    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        if let Some((label, toks)) = Label::parse(toks, ctx) {
            let (stmt, toks) = Self::parse(toks, ctx)?;
            return Some((
                Self {
                    loc: Location::from_merged(&fst.loc, &stmt.loc),
                    attr: None,
                    stmt: StmtType::Labeled(label, Box::new(stmt)),
                },
                toks,
            ));
        }
        if let Some((d, toks)) = BlockDeclaration::parse(toks, ctx) {
            return Some((
                Self {
                    stmt: StmtType::BlockDecl(d),
                    loc: Location::new("dummy", 0, 0), // TODO:
                    attr: None,
                },
                toks,
            ));
        }
        let (attr, toks) = parse_opt(toks, ctx, Attr::parse);
        let fst = toks.first()?;
        if fst.tok.is_punct("{") {
            Self::parse_compound_with_attr(attr, toks, ctx)
        } else if fst.tok.is_kw("if") {
            Self::parse_if(attr, toks, ctx)
        } else if fst.tok.is_kw("switch") {
            Self::parse_switch(attr, toks, ctx)
        } else if fst.tok.is_kw("while") {
            Self::parse_while(attr, toks, ctx)
        } else if fst.tok.is_kw("do") {
            Self::parse_dowhile(attr, toks, ctx)
        } else if fst.tok.is_kw("for") {
            Self::parse_for(attr, toks, ctx)
        } else if fst.tok.is_kw("break") {
            let semi = toks[1..].first()?;
            semi.check_punct(";");
            let loc = Location::from_merged(
                if let Some(a) = &attr {
                    &a.loc
                } else {
                    &fst.loc
                },
                &semi.loc,
            );
            Some((
                Self {
                    loc,
                    attr,
                    stmt: StmtType::Break,
                },
                &toks[2..],
            ))
        } else if fst.tok.is_kw("continue") {
            let semi = toks[1..].first()?;
            semi.check_punct(";");
            let loc = Location::from_merged(
                if let Some(a) = &attr {
                    &a.loc
                } else {
                    &fst.loc
                },
                &semi.loc,
            );
            Some((
                Self {
                    loc,
                    attr,
                    stmt: StmtType::Continue,
                },
                &toks[2..],
            ))
        } else if fst.tok.is_kw("return") {
            Self::parse_return(attr, toks, ctx)
        } else if fst.tok.is_kw("goto") {
            let id = toks[1..].first()?.tok.ident()?;
            let semi = toks[2..].first()?;
            semi.check_punct(";");
            let loc = Location::from_merged(
                if let Some(a) = &attr {
                    &a.loc
                } else {
                    &fst.loc
                },
                &semi.loc,
            );
            Some((
                Self {
                    loc,
                    attr,
                    stmt: StmtType::Goto(id),
                },
                &toks[3..],
            ))
        } else if fst.tok.is_kw("try") {
            Self::parse_try(attr, toks, ctx)
        } else if fst.tok.is_punct(";") {
            let semi = toks.first().unwrap();
            let loc = if let Some(a) = &attr {
                Location::from_merged(&a.loc, &semi.loc)
            } else {
                fst.loc.clone()
            };
            Some((
                Self {
                    stmt: StmtType::Null,
                    loc,
                    attr,
                },
                &toks[1..],
            ))
        } else {
            Self::parse_expr(attr, toks, ctx)
        }
    }

    fn print_ast_inline(&self) -> String {
        let mut out = String::new();
        match &self.stmt {
            StmtType::If(c, _, _, _, s2) => {
                if *c {
                    out.push_str(" constexpr");
                }
                if s2.is_some() {
                    out.push_str(" has_else");
                }
            }
            StmtType::Labeled(l, _) => {
                out.push(' ');
                out.push_str(&l.ast_str());
            }
            StmtType::Goto(l) => {
                out.push(' ');
                out.push_str(&l);
            }
            StmtType::IfConsteval(n, _, _) => {
                if *n {
                    out.push('!');
                }
                out.push_str("consteval");
            }
            _ => {}
        }
        out
    }

    fn print_ast_child(&self, lstr: &str) -> Option<String> {
        use ast_printing::AstPrint;
        match &self.stmt {
            StmtType::Compound(c, _) => ast_printing::print_ast_list_opt(c, lstr),
            StmtType::Return(Some(e)) => Some(e.print_ast_end(lstr)),
            StmtType::ReturnInit(i) => Some(format!("{i:?}")),
            StmtType::If(_, i, e, s1, s2) => {
                let mut out = String::new();
                if let Some(i) = i {
                    out.push_str(&i.print_ast_mid(lstr));
                    out.push('\n');
                }
                out.push_str(&e.print_ast_mid(lstr));
                out.push('\n');
                if let Some(s2) = s2 {
                    out.push_str(&s1.print_ast_mid(lstr));
                    out.push('\n');
                    out.push_str(&s2.print_ast_end(lstr));
                } else {
                    out.push_str(&s1.print_ast_end(lstr));
                }

                Some(out)
            }
            StmtType::IfConsteval(_, s1, s2) => {
                let mut out = String::new();
                if let Some(s2) = s2 {
                    out.push_str(&s1.print_ast_mid(lstr));
                    out.push('\n');
                    out.push_str(&s2.print_ast_end(lstr));
                } else {
                    out.push_str(&s1.print_ast_end(lstr));
                }
                Some(out)
            }
            StmtType::Switch(init, e, s) => {
                let mut out = String::new();
                if let Some(init) = init {
                    out.push_str(&init.print_ast_mid(lstr));
                    out.push('\n');
                }
                out.push_str(&e.print_ast_mid(lstr));
                out.push('\n');
                out.push_str(&s.print_ast_end(lstr));
                Some(out)
            }
            StmtType::While(e, s) | StmtType::DoWhile(e, s) => Some(format!("{}\n{}", e.print_ast_mid(lstr), s.print_ast_end(lstr))),
            StmtType::For(init, cond, post, stmt) => {
                let mut out = String::new();
                out.push_str(&init.print_ast_mid(lstr));
                out.push('\n');
                if let Some(cond) = cond {
                    out.push_str(&cond.print_ast_mid(lstr));
                    out.push('\n');
                }
                if let Some(post) = post {
                    out.push_str(&post.print_ast_mid(lstr));
                    out.push('\n');
                }
                out.push_str(&stmt.print_ast_end(lstr));
                Some(out)
            }
            StmtType::Labeled(l, s) => {
                let mut out = String::new();
                if let Label::Case(_, e) = &l {
                    out.push_str(&e.print_ast_mid(lstr));
                    out.push('\n');
                }
                out.push_str(&s.print_ast_end(lstr));
                Some(out)
            }
            StmtType::Try(s, h) => {
                let mut out = String::new();
                out.push_str(&s.print_ast_mid(lstr));
                out.push('\n');
                out.push_str(&ast_printing::print_ast_list(h, lstr));
                Some(out)
            }
            _ => None,
        }
    }
}

impl ast_printing::AstPrint for Stmt {
    fn print_ast(&self, fstr: &str, lstr: &str) -> String {
        match &self.stmt {
            StmtType::Expr(e) => return e.print_ast(fstr, lstr),
            StmtType::BlockDecl(d) => return d.print_ast(fstr, lstr),
            _ => {}
        }
        let mut out = ast_printing::start_str_with_loc(color::PURPLE, self.stmt.name(), fstr, &self.loc);
        out.push_str(&self.print_ast_inline());
        let c = self.print_ast_child(lstr);
        if let Some(c) = c {
            if let Some(a) = &self.attr {
                out.push('\n');
                out.push_str(&a.print_ast_mid(lstr));
            }
            out.push('\n');
            out.push_str(&c);
        } else if let Some(a) = &self.attr {
            out.push('\n');
            out.push_str(&a.print_ast_end(lstr));
        }
        out
    }
}
