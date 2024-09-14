mod lambda;

use lambda::LambdaExpr;

use crate::{
    parsing::{
        ast::parse_util::{self, parse_sep_elt_list_nosep, ParseFun},
        context::Context,
        LitType, PunctType, TokType, Token,
    },
    util::{ast_printing, color, loc::Location},
};

use super::{parse_util::parse_sep_comma, Type};

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

    fn check(tok: &Token) -> Option<Self> {
        let TokType::Punct(p) = tok.tok else { return None; };
        Some(match p {
            PunctType::PlusEq => Self::Add,
            PunctType::MinusEq => Self::Sub,
            PunctType::SlashEq => Self::Div,
            PunctType::StarEq => Self::Mul,
            PunctType::PercentEq => Self::Mod,
            PunctType::AndEq => Self::BinAnd,
            PunctType::OrEq => Self::BinOr,
            PunctType::XorEq => Self::BinXor,
            PunctType::LshiftEq => Self::LShift,
            PunctType::RshiftEq => Self::RShift,
            _ => return None,
        })
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
    // TODO: parensed operators
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

    fn check(toks: &[Token], ctx: &mut Context) -> Option<Self> {
        let tok = toks.first()?;
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
        } else if tok.tok.is_kw("sizeof") {
            if check_parensed_type(&toks[1..], ctx).is_some() {
                return None
            }
            UnaryOp::Sizeof
        } else {
            return None;
        })
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
            ExprType::MemberPtr {member, ..} => out.push_str(&color::format_str().s_cb(&format!("'->{}'", member), color::GREEN).build()),
            ExprType::Lambda(l) => out.push_str(&format!("\n{}", l.end.body.print_ast_end(lstr))),
            ExprType::CStyleCast{ty, ..} => out.push_str(&color::format_str().s_cb(&format!("'{}'", ty.ty_str()), color::GREEN).build()),
            _ => {}
        };
        match &self.expr {
            ExprType::Binary { op: _, left, right }
            | ExprType::CompoundAssign { op: _, left, right }
            | ExprType::Assign { left, right } => {
                out.push('\n');
                out.push_str(&left.print_ast_mid(lstr));
                out.push('\n');
                out.push_str(&right.print_ast_end(lstr));
            }
            ExprType::Unary {inner: a, ..} | ExprType::Member {left: a, ..}
            | ExprType::MemberPtr {left: a, ..} | ExprType::CStyleCast {val: a, ..}
            | ExprType::CoAwait(a) | ExprType::CoYield(a) | ExprType::Throw(a) => {
                out.push('\n');
                out.push_str(&a.print_ast_end(lstr));
            }
            ExprType::Ternary { cond, iftrue, iffalse } => {
                out.push('\n');
                out.push_str(&cond.print_ast_mid(lstr));
                out.push('\n');
                out.push_str(&iftrue.print_ast_mid(lstr));
                out.push('\n');
                out.push_str(&iffalse.print_ast_end(lstr));
            }
            ExprType::FCall { left, args } | ExprType::Subscript { left, args } => {
                out.push('\n');
                if args.is_empty() {
                    out.push_str(&left.print_ast_end(lstr));
                } else {
                    out.push_str(&left.print_ast_mid(lstr));
                    out.push('\n');
                    out.push_str(&ast_printing::print_ast_list(&args, lstr));
                }
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
    Unary {
        op: UnaryOp,
        inner: Box<Expr>,
    },
    Ternary {
        cond: Box<Expr>,
        iftrue: Box<Expr>,
        iffalse: Box<Expr>,
    },
    Assign {
        left: Box<Expr>,
        right: Box<Expr>,
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
    MemberPtr {
        left: Box<Expr>,
        member: String,
    },
    FCall {
        left: Box<Expr>,
        args: Vec<Expr>,
    },
    Subscript {
        left: Box<Expr>,
        args: Vec<Expr>,
    },
    CStyleCast {
        ty: Type,
        val: Box<Expr>,
    },
    Litteral(LitType),
    Ident(String),
    Lambda(LambdaExpr),
    CoAwait(Box<Expr>),
    CoYield(Box<Expr>),
    Throw(Box<Expr>),
    // TODO: new+delete+new[]+delete[]
}

impl ExprType {
    fn name(&self) -> &'static str {
        match &self {
            ExprType::Litteral(..) => "LitteralExpr",
            ExprType::Ident(..) => "IdentExpr",
            ExprType::Binary{..} => "BinaryOpExpr",
            ExprType::Unary{..} => "UnaryOpExpr",
            ExprType::CompoundAssign{..} => "CompoundAssignExpr",
            ExprType::Assign{..} => "AssignExpr",
            ExprType::Member{..} => "MemberAccessExpr",
            ExprType::MemberPtr{..} => "MemberAccessExpr",
            ExprType::Lambda{..} => "LambdaExpr",
            ExprType::FCall{..} => "FunctionCallExpr",
            ExprType::Subscript{..} => "SubscriptExpr",
            ExprType::Throw(..) => "ThrowExpr",
            ExprType::Ternary{..} => "TernaryOpExpr",
            ExprType::CoAwait(..) => "CoAwaitExpr",
            ExprType::CoYield(..) => "CoYieldExpr",
            ExprType::CStyleCast{..} => "CStyleCastExpr",
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
    let fst = toks.first()?;
    if fst.tok.is_kw("throw") {
        let (e, toks) = parse_expr_16(&toks[1..], ctx)?;
        return Some((
            Expr {
                loc: Location::from_merged(&fst.loc, &e.loc),
                expr: ExprType::Throw(Box::new(e)),
            }, toks
        ))
    } else if fst.tok.is_kw("co_yield") {
        let (e, toks) = parse_expr_16(&toks[1..], ctx)?;
        return Some((
            Expr {
                loc: Location::from_merged(&fst.loc, &e.loc),
                expr: ExprType::CoYield(Box::new(e)),
            }, toks
        ))
    }
    let (left, toks) = parse_expr_15(toks, ctx)?;
    if toks.is_empty() {
        return Some((left, toks));
    }
    let fst = toks.first()?;
    if fst.tok.is_punct("?") {
        let (iftrue, toks) = parse_expr_16(&toks[1..], ctx)?;
        toks.first()?.check_punct(":")?;
        let (iffalse, toks) = parse_expr_16(&toks[1..], ctx)?;
        Some((Expr {
            loc: Location::from_merged(&left.loc, &iffalse.loc),
            expr: ExprType::Ternary { cond: Box::new(left), iftrue: Box::new(iftrue), iffalse: Box::new(iffalse) }
        }, toks))
    } else if fst.tok.is_punct("=") {
        let (right, toks) = parse_expr_16(&toks[1..], ctx)?;
        Some((Expr {
            loc: Location::from_merged(&left.loc, &right.loc),
            expr: ExprType::Assign { left: Box::new(left), right: Box::new(right) },
        }, toks))
    } else if let Some(op) = CompoundAssignOp::check(fst) {
        let (right, toks) = parse_expr_16(&toks[1..], ctx)?;
        Some((Expr {
            loc: Location::from_merged(&left.loc, &right.loc),
            expr: ExprType::CompoundAssign { op, left: Box::new(left), right: Box::new(right) },
        }, toks))
    } else {
        Some((left, toks))
    }
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
    // I still don't understand what this is
    parse_expr_3(toks, ctx)
}

fn parse_expr_3<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    let first = toks.first()?;
    if let Some(op) = UnaryOp::check(toks, ctx) {
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
    } else if first.tok.is_kw("co_await") {
        let (expr, toks) = parse_expr_3(&toks[1..], ctx)?;
        Some((
            Expr {
                loc: Location::from_merged(&first.loc, &expr.loc),
                expr: ExprType::CoAwait(Box::new(expr)),
            },
            toks,
        ))
    } else if let Some((ty, toks)) = check_parensed_type(toks, ctx) {
        let (expr, toks) = parse_expr_3(toks, ctx)?;
        Some((Expr {
                loc: Location::from_merged(&first.loc, &expr.loc),
                expr: ExprType::CStyleCast { ty, val: Box::new(expr) }
        }, toks))
    } else {
        // TODO: new, new[], delete, delete[]
        parse_expr_2(toks, ctx)
    }
}

fn parse_expr_2<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    // TODO: Functionnal cast

    let (mut a, mut toks_it) = parse_expr_1(toks, ctx)?;

    loop {
        if toks_it.is_empty() {
            break;
        }
        let fst = toks_it.first().unwrap();
        if fst.tok.is_punct("++") {
            (a, toks_it) = (Expr {
                loc: Location::from_merged(&a.loc , &fst.loc),
                expr: ExprType::Unary { op: UnaryOp::PostIncr, inner: Box::new(a) },
            }, &toks[1..]);
        } else if fst.tok.is_punct("--") {
            (a, toks_it) = (Expr {
                loc: Location::from_merged(&a.loc , &fst.loc),
                expr: ExprType::Unary { op: UnaryOp::PostDecr, inner: Box::new(a) },
            }, &toks_it[1..]);
        } else if fst.tok.is_punct("(") {
            let (sub_exprs, toks) = parse_sep_elt_list_nosep(&toks_it[1..], ctx, Expr::parse_no_comma, parse_sep_comma)?;
            let lst = toks.first()?;
            lst.check_punct(")")?;
            (a, toks_it) = (Expr {
                loc: Location::from_merged(&a.loc , &lst.loc),
                expr: ExprType::FCall { left: Box::new(a), args: sub_exprs }
            }, &toks[1..]);
        } else if fst.tok.is_punct("[") {
            let (toks, has_brace) = if toks_it[1..].first()?.tok.is_punct("{") {
                (&toks_it[1..], true)
            } else {
                (toks_it, false)
            };
            let (sub_exprs, toks) = parse_sep_elt_list_nosep(&toks[1..], ctx, Expr::parse_no_comma, parse_sep_comma)?;
            let toks = if has_brace {
                toks.first()?.check_punct("}")?;
                &toks[1..]
            } else {
                toks
            };
            let lst = toks.first()?;
            lst.check_punct("]")?;
            (a, toks_it) = (Expr {
                loc: Location::from_merged(&a.loc , &lst.loc),
                expr: ExprType::Subscript { left: Box::new(a), args: sub_exprs }
            }, &toks[1..]);
        } else if fst.tok.is_punct(".") {
            let fst = toks_it[1..].first()?;
            let id = fst.tok.ident()?;
            (a, toks_it) = (Expr {
                loc: Location::from_merged(&a.loc, &fst.loc),
                expr: ExprType::Member { left: Box::new(a), member: id },
            }, &toks_it[2..]);
        } else if fst.tok.is_punct("->") {
            let fst = toks_it[1..].first()?;
            let id = fst.tok.ident()?;
            (a, toks_it) = (Expr {
                loc: Location::from_merged(&a.loc, &fst.loc),
                expr: ExprType::MemberPtr { left: Box::new(a), member: id },
            }, &toks_it[2..]);
        } else {
            break;
        }
    }

    Some((a, toks_it))
}

fn parse_expr_1<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Expr, &'a [Token])> {
    // TODO: Full Exact
    // - missing parensed operators (like alignof sizeof ...)
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
            if *p == PunctType::LBracket {
                let (l, t) = LambdaExpr::parse(toks, ctx)?;
                return Some((
                    Expr {
                        loc: l.loc.clone(),
                        expr: ExprType::Lambda(l),
                    }, t));
            }
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

fn check_parensed_type<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Type, &'a [Token])> {
    toks.first()?.check_punct("(")?;
    let (ty, toks) = Type::parse(&toks[1..], ctx)?;
    toks.first()?.check_punct(")")?;
    Some((ty, toks))
}
