use crate::{
    parsing::{context::Context, Token},
    util::loc::Location,
};

pub type ParseFun<'a, T> = fn(&'a [Token], &mut Context) -> Option<(T, &'a [Token])>;

pub fn balanced_tok_seq<'a>(
    toks: &'a [Token],
    _ctx: &mut Context,
) -> Option<(Vec<Token>, &'a [Token])> {
    let mut out = Vec::new();

    let mut par_stack = Vec::new();
    let mut toks = toks;
    while let Some(t) = toks.first() {
        if t.tok.is_punct("(") {
            par_stack.push('(');
        } else if t.tok.is_punct("{") {
            par_stack.push('{');
        } else if t.tok.is_punct("[") {
            par_stack.push('[');
        } else if t.tok.is_punct(")") {
            if let Some(p) = par_stack.pop() {
                if p != '(' {
                    panic!("unbalanced token sequence");
                }
            } else {
                return Some((out, toks));
            }
        } else if t.tok.is_punct("]") {
            if let Some(p) = par_stack.pop() {
                if p != '[' {
                    panic!("unbalanced token sequence");
                }
            } else {
                return Some((out, toks));
            }
        } else if t.tok.is_punct("}") {
            if let Some(p) = par_stack.pop() {
                if p != '{' {
                    panic!("unbalanced token sequence");
                }
            } else {
                return Some((out, toks));
            }
        }
        out.push(t.clone());
        toks = &toks[1..];
    }
    None
}

pub fn parse_elt_list<'a, T>(
    toks: &'a [Token],
    ctx: &mut Context,
    f: ParseFun<'a, T>,
) -> Option<(Vec<T>, &'a [Token])> {
    let mut toks = toks;
    let mut out = Vec::new();
    while !toks.is_empty() {
        if let Some((o, rt)) = f(toks, ctx) {
            toks = rt;
            out.push(o);
        } else {
            break;
        }
    }
    Some((out, toks))
}

pub fn parse_non_empty_elt_list<'a, T>(
    toks: &'a [Token],
    ctx: &mut Context,
    f: ParseFun<'a, T>,
) -> Option<(Vec<T>, &'a [Token])> {
    let (out, toks) = parse_elt_list(toks, ctx, f)?;
    if out.is_empty() {
        None
    } else {
        Some((out, toks))
    }
}

pub fn parse_opt<'a, T>(
    toks: &'a [Token],
    ctx: &mut Context,
    f: ParseFun<'a, T>,
) -> (Option<T>, &'a [Token]) {
    if let Some((o, toks)) = f(toks, ctx) {
        (Some(o), toks)
    } else {
        (None, toks)
    }
}

pub fn parse_sep_comma<'a>(toks: &'a [Token], _: &mut Context) -> Option<((), &'a [Token])> {
    toks.first()?.check_punct(",")?;
    Some(((), &toks[1..]))
}

pub enum SepListEntry<T, S> {
    Val(T),
    Sep(S),
}

impl<T, S> SepListEntry<T, S> {
    pub fn val(self) -> T {
        let Self::Val(v) = self else {
            panic!();
        };
        v
    }
    pub fn sep(self) -> S {
        let Self::Sep(s) = self else {
            panic!();
        };
        s
    }
}

pub type ParseSepListResult<'a, T, S> = (Vec<SepListEntry<T, S>>, &'a [Token]);

pub fn parse_sep_elt_list<'a, T, S>(
    toks: &'a [Token],
    ctx: &mut Context,
    vf: ParseFun<'a, T>,
    sf: ParseFun<'a, S>,
) -> Option<ParseSepListResult<'a, T, S>> {
    let mut toks = toks;
    let mut out = Vec::new();
    if let Some((o, rt)) = vf(toks, ctx) {
        toks = rt;
        out.push(SepListEntry::Val(o));
    } else {
        return Some((out, toks));
    }
    while !toks.is_empty() {
        if let Some((o, rt)) = sf(toks, ctx) {
            toks = rt;
            out.push(SepListEntry::Sep(o));
        } else {
            break;
        }
        let (o, rt) = vf(toks, ctx).expect("Expected seplist entry");
        out.push(SepListEntry::Val(o));
        toks = rt;
    }
    Some((out, toks))
}

pub fn parse_sep_elt_list_nosep<'a, T, S>(
    toks: &'a [Token],
    ctx: &mut Context,
    vf: ParseFun<'a, T>,
    sf: ParseFun<'a, S>,
) -> Option<(Vec<T>, &'a [Token])> {
    let mut toks = toks;
    let mut out = Vec::new();
    if let Some((o, rt)) = vf(toks, ctx) {
        toks = rt;
        out.push(o);
    } else {
        return Some((out, toks));
    }
    while !toks.is_empty() {
        if let Some((_, rt)) = sf(toks, ctx) {
            toks = rt;
        } else {
            break;
        }
        let (o, rt) = vf(toks, ctx).expect("Expected seplist entry");
        out.push(o);
        toks = rt;
    }
    Some((out, toks))
}

pub fn parse_non_empty_sep_elt_list_nosep<'a, T, S>(
    toks: &'a [Token],
    ctx: &mut Context,
    vf: ParseFun<'a, T>,
    sf: ParseFun<'a, S>,
) -> Option<(Vec<T>, &'a [Token])> {
    let (out, toks) = parse_sep_elt_list_nosep(toks, ctx, vf, sf)?;
    if out.is_empty() {
        None
    } else {
        Some((out, toks))
    }
}

pub fn parse_non_empty_sep_elt_list<'a, T, S>(
    toks: &'a [Token],
    ctx: &mut Context,
    vf: ParseFun<'a, T>,
    sf: ParseFun<'a, S>,
) -> Option<ParseSepListResult<'a, T, S>> {
    let (out, toks) = parse_sep_elt_list(toks, ctx, vf, sf)?;
    if out.is_empty() {
        None
    } else {
        Some((out, toks))
    }
}

pub fn sep_list_remove_sep<T, S>(list: Vec<SepListEntry<T, S>>) -> Vec<T> {
    list.into_iter()
        .filter_map(|item| match item {
            SepListEntry::Val(v) => Some(v),
            SepListEntry::Sep(_) => None,
        })
        .collect()
}
