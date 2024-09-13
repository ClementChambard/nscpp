use crate::{
    parsing::{
        ast::parse_util::{self, ParseFun},
        context::Context,
        LitType, PunctType, TokType, Token,
    },
    util::{ast_printing, color, loc::Location},
};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    And,
    Or,
    BinAnd,
    BinOr,
    BinXor,
    LShift,
    RShift,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    Spaceship,
    Assign,
    Comma,
}

impl BinaryOp {
    fn get_str(&self) -> &'static str {
        match &self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Div => "/",
            Self::Mul => "*",
            Self::Mod => "%",
            Self::And => "&&",
            Self::Or => "||",
            Self::BinAnd => "&",
            Self::BinOr => "|",
            Self::BinXor => "^",
            Self::LShift => "<<",
            Self::RShift => ">>",
            Self::Eq => "==",
            Self::Ne => "!=",
            Self::Gt => ">",
            Self::Lt => "<",
            Self::Ge => ">=",
            Self::Le => "<=",
            Self::Spaceship => "<=>",
            Self::Assign => "=",
            Self::Comma => ",",
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CompoundAssignOp {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    BinAnd,
    BinOr,
    BinXor,
    LShift,
    RShift,
}

impl CompoundAssignOp {
    fn get_str(&self) -> &'static str {
        match &self {
            Self::Add => "+=",
            Self::Sub => "-=",
            Self::Div => "/=",
            Self::Mul => "*=",
            Self::Mod => "%=",
            Self::BinAnd => "&=",
            Self::BinOr => "|=",
            Self::BinXor => "^=",
            Self::LShift => "<<=",
            Self::RShift => ">>=",
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    Compl,
    Addr,
    Deref,
    Sizeof,
    PreIncr,
    PostIncr,
    PreDecr,
    PostDecr,
}

impl UnaryOp {
    fn get_str(&self) -> &'static str {
        match &self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Not => "!",
            Self::Compl => "~",
            Self::Addr => "&",
            Self::Deref => "*",
            Self::Sizeof => "sizeof",
            Self::PreIncr => "++",
            Self::PostIncr => "++ post",
            Self::PreDecr => "--",
            Self::PostDecr => "-- post",
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Expr {
    pub expr: ExprType,
    pub loc: Location,
}

impl Expr {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        parse_expr_17(toks, ctx)
    }

    pub fn parse_no_comma<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        parse_expr_16(toks, ctx)
    }
}

impl ast_printing::AstPrint for Expr {
    fn print_ast(&self, fstr: &str, lstr: &str) -> String {
        let mut out = ast_printing::start_str_with_loc(color::PURPLE, self.expr.name(), fstr, &self.loc);
        out.push(' ');
        match &self.expr {
            ExprType::Litteral(l) => out.push_str(&color::format_str().put_dcb(l, color::CYAN).build()),
            ExprType::Ident(i) => out.push_str(&color::format_str().s_cb(&i, color::CYAN).build()),
            ExprType::Binary {op, ..} => out.push_str(&color::format_str().s_cb(&format!("'{}'", op.get_str()), color::GREEN).build()),
            ExprType::Unary {op, ..} => out.push_str(&color::format_str().s_cb(&format!("'{}'", op.get_str()), color::GREEN).build()),
            ExprType::CompoundAssign {op, ..} => out.push_str(&color::format_str().s_cb(&format!("'{}'", op.get_str()), color::GREEN).build()),
            ExprType::Member {member, ..} => out.push_str(&color::format_str().s_cb(&format!("'.{}'", member), color::GREEN).build()),
        };
        match &self.expr {
            ExprType::Binary { op: _, left, right }
            | ExprType::CompoundAssign { op: _, left, right } => {
                out.push('\n');
                out.push_str(&left.print_ast_mid(lstr));
                out.push('\n');
                out.push_str(&right.print_ast_end(lstr));
            }
            ExprType::Unary { op: _, inner: a } | ExprType::Member { left: a, member: _ } => {
                out.push('\n');
                out.push_str(&a.print_ast_end(lstr));
            }
            _ => {}
        }
        out
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum ExprType {
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    // TODO:
    Unary {
        op: UnaryOp,
        inner: Box<Expr>,
    },
    CompoundAssign {
        op: CompoundAssignOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Member {
        left: Box<Expr>,
        member: String,
    },
    Litteral(LitType),
    Ident(String),
    // TODO:
}

impl ExprType {
    fn name(&self) -> &'static str {
        match &self {
            ExprType::Litteral(..) => "LitteralExpr",
            ExprType::Ident(..) => "IdentExpr",
            ExprType::Binary{..} => "BinaryOpExpr",
            ExprType::Unary{..} => "UnaryOpExpr",
            ExprType::CompoundAssign{..} => "CompoundAssignExpr",
            ExprType::Member{..} => "MemberAccessExpr",
            _ => "",
        }
    }
}

fn parse_bin_op_expr_ltr<'a>(
    toks: &'a [Token],
    ctx: &mut Context,
    elt: ParseFun<'a, Expr>,
    sep: ParseFun<'a, BinaryOp>,
) -> Option<(Expr, &'a [Token])> {
    let (v, toks) = parse_util::parse_sep_elt_list(toks, ctx, elt, sep)?;
    if v.is_empty() {
        return None;
    }
    let mut it = v.into_iter();
    let mut left = it.next().unwrap().val();
    while let Some(op) = it.next() {
        let right = it.next().unwrap().val();
        left = Expr {
            loc: Location::from_merged(&left.loc, &right.loc),
            expr: ExprType::Binary {
                op: op.sep(),
                left: Box::new(left),
                right: Box::new(right),
            },
        }
    }
    Some((left, toks))
}

fn parse_expr_17<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct(",") {
                return Some((BinaryOp::Comma, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_16, parse_sep)
}

fn parse_expr_16<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    // TODO:
    // assignment                 // RTL
    // all compound assignments
    // co_yield, throw
    // ternary
    parse_expr_15(toks, ctx)
}

fn parse_expr_15<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct("||") {
                return Some((BinaryOp::Or, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_14, parse_sep)
}

fn parse_expr_14<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct("&&") {
                return Some((BinaryOp::And, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_13, parse_sep)
}

fn parse_expr_13<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct("|") {
                return Some((BinaryOp::BinOr, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_12, parse_sep)
}

fn parse_expr_12<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct("^") {
                return Some((BinaryOp::BinXor, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_11, parse_sep)
}

fn parse_expr_11<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct("&") {
                return Some((BinaryOp::BinAnd, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_10, parse_sep)
}

fn parse_expr_10<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct("==") {
                return Some((BinaryOp::Eq, &toks[1..]));
            } else if t.tok.is_punct("!=") {
                return Some((BinaryOp::Ne, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_9, parse_sep)
}

fn parse_expr_9<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct(">") {
                return Some((BinaryOp::Gt, &toks[1..]));
            } else if t.tok.is_punct("<") {
                return Some((BinaryOp::Lt, &toks[1..]));
            } else if t.tok.is_punct(">=") {
                return Some((BinaryOp::Ge, &toks[1..]));
            } else if t.tok.is_punct("<=") {
                return Some((BinaryOp::Le, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_8, parse_sep)
}

fn parse_expr_8<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct("<=>") {
                return Some((BinaryOp::Spaceship, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_7, parse_sep)
}

fn parse_expr_7<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct("<<") {
                return Some((BinaryOp::LShift, &toks[1..]));
            } else if t.tok.is_punct(">>") {
                return Some((BinaryOp::RShift, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_6, parse_sep)
}

fn parse_expr_6<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct("+") {
                return Some((BinaryOp::Add, &toks[1..]));
            } else if t.tok.is_punct("-") {
                return Some((BinaryOp::Sub, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_5, parse_sep)
}

fn parse_expr_5<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    fn parse_sep<'a>(toks: &'a [Token], _: &mut Context) -> Option<(BinaryOp, &'a [Token])> {
        if let Some(t) = toks.iter().next() {
            if t.tok.is_punct("*") {
                return Some((BinaryOp::Mul, &toks[1..]));
            } else if t.tok.is_punct("/") {
                return Some((BinaryOp::Div, &toks[1..]));
            } else if t.tok.is_punct("%") {
                return Some((BinaryOp::Mod, &toks[1..]));
            }
        }
        None
    }
    parse_bin_op_expr_ltr(toks, ctx, parse_expr_4, parse_sep)
}

fn parse_expr_4<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    // TODO:
    // ptr to member              // LTR
    parse_expr_3(toks, ctx)
}

fn get_pre_unary_op(tok: &Token) -> Option<UnaryOp> {
    Some(if tok.tok.is_punct("++") {
        UnaryOp::PreIncr
    } else if tok.tok.is_punct("+") {
        UnaryOp::Plus
    } else if tok.tok.is_punct("--") {
        UnaryOp::PreDecr
    } else if tok.tok.is_punct("-") {
        UnaryOp::Minus
    } else if tok.tok.is_punct("!") {
        UnaryOp::Not
    } else if tok.tok.is_punct("~") {
        UnaryOp::Compl
    } else if tok.tok.is_punct("*") {
        UnaryOp::Deref
    } else if tok.tok.is_punct("&") {
        UnaryOp::Addr
    } else {
        return None;
    })
}

fn parse_expr_3<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    let first = toks.first()?;
    if let Some(op) = get_pre_unary_op(first) {
        let (expr, toks) = parse_expr_3(&toks[1..], ctx)?;
        Some((
            Expr {
                loc: Location::from_merged(&first.loc, &expr.loc),
                expr: ExprType::Unary {
                    op,
                    inner: Box::new(expr),
                },
            },
            toks,
        ))
    } else {
        // TODO:                      // RTL
        // CStyle cast         ( kw/id ) a
        // sizeof, co_await
        // new, new[], delete, delete[]
        parse_expr_2(toks, ctx)
    }
}

fn parse_expr_2<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    // TODO:
    // post incr, dect  a ++  or  a --
    // functional cast  kw ( b )  or  a ( b )
    // function call    a ( b... )
    // subscript        a [ b... ]
    // member access    a . b
    parse_expr_1(toks, ctx)
}

fn parse_expr_1<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    // TODO:
    // scope resolution  a :: b
    parse_expr_0(toks, ctx)
}

fn parse_expr_0<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    // TODO: Full Exact
    let tok = toks.first()?;
    match &tok.tok {
        TokType::Lit(l) => Some((
            Expr {
                expr: ExprType::Litteral(l.clone()),
                loc: tok.loc.clone(),
            },
            &toks[1..],
        )),
        TokType::Id(i) => Some((
            Expr {
                expr: ExprType::Ident(i.clone()),
                loc: tok.loc.clone(),
            },
            &toks[1..],
        )),
        TokType::Punct(p) => {
            if *p != PunctType::LParens {
                return None;
            }
            let (e, toks) = parse_expr_17(&toks[1..], ctx)?;
            let p2 = toks.first()?;
            if let TokType::Punct(p) = p2.tok {
                if p != PunctType::RParens {
                    return None;
                }
                Some((
                    Expr {
                        loc: Location::from_merged(&tok.loc, &p2.loc),
                        expr: e.expr,
                    },
                    &toks[1..],
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}

// TO PARSE EXPR:
// PARSE LIST OF EXPR_LVL1
// EXPR_LVL1 = LIST OF EXPR_LVL2 ...
// EXPR_LVLMAX =
