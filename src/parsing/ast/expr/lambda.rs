use crate::{parsing::{ast::{attr::Attr, decl::{block_decl::Initializer, Noexcept, ParamDecl}, parse_util::{parse_elt_list, parse_opt, parse_sep_comma, parse_sep_elt_list_nosep}, Stmt, Type}, context::Context, Token}, util::loc::Location};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum CaptureDefault {
    Ref,                                // '&'
    Copy,                               // '='
    None,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Capture {
    Copy(String),                       // id
    PackCopy(String),                   // id '...'
    CopyInit(String, Initializer),      // id Initializer
    Ref(String),                        // '&' id
    PackRef(String),                    // '&' id '...'
    RefInit(String, Initializer),       // '&' id Initializer
    This,                               // 'this'
    CopyThis,                           // '*' 'this'
    PackCopyInit(String, Initializer),  // '...' id Initializer
    PackRefInit(String, Initializer),   // '&' '...' id Initializer
}

impl Capture {
    fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let mut fst = toks.first()?;
        if fst.tok.is_punct("*") {
            if toks[1..].first()?.tok.is_kw("this") {
                return Some((Self::CopyThis, &toks[2..]));
            } else {
                // TODO: Should error if [1] is None
                panic!("Error in Capture::parse: expected 'this' after '*' ");
            }
        } else if fst.tok.is_kw("this") {
            return Some((Self::This, &toks[1..]));
        }
        let mut is_ref = false;
        let mut toks = toks;
        if fst.tok.is_punct("&") {
            is_ref = true;
            toks = &toks[1..];
            fst = toks.first().expect("Error in Capture::parse: expected token after '&' ");
        }
        if fst.tok.is_punct("...") {
            // PackInit
            let id = toks[1..].first().expect("TODO: message").tok.ident().expect("TODO: message");
            let (init, toks) = Initializer::parse(toks, ctx).expect("TODO: message");
            return Some((if is_ref {
                Self::PackRefInit(id, init)
            } else {
                Self::PackCopyInit(id, init)
            }, toks));
        }
        let id = fst.tok.ident().expect("TODO: message");
        if let Some((init, toks)) = Initializer::parse(&toks[1..], ctx) {
            Some((if is_ref {
                Self::RefInit(id, init)
            } else {
                Self::CopyInit(id, init)
            }, toks))
        } else if toks[1..].first().expect("TODO: message (EOF not in follows)").tok.is_punct("...") {
            Some((if is_ref {
                Self::PackRef(id)
            } else {
                Self::PackCopy(id)
            }, &toks[2..]))
        } else {
            Some((if is_ref {
                Self::Ref(id)
            } else {
                Self::Copy(id)
            }, &toks[1..]))
        }
    }

    fn check_default(&self, _d: CaptureDefault) -> bool {
        // TODO:
        true
    }
}

// Captures ::= '[' SepList<Capture, ','> ']' ;
//            | '[' CaptureDefault ']'
//            | '[' CaptureDefault ',' NonEmptySepList<Capture, ','> ']'
//            ;
#[derive(Clone, PartialEq, Debug)]
pub struct Captures {
    pub cdef: CaptureDefault,
    pub captures: Vec<Capture>,
}

impl Captures {
    fn get_capture_default(toks: &[Token]) -> (CaptureDefault, &[Token]) {
        if let Some(fst) =  toks.first() {
            if fst.tok.is_punct("&") {
                if let Some(snd) = toks[1..].first() {
                    if snd.tok.is_punct("]") || snd.tok.is_punct(",") {
                        return (CaptureDefault::Ref, &toks[1..])
                    }
                }
            } else if fst.tok.is_punct("=") {
                return (CaptureDefault::Copy, &toks[1..]);
            }
        }
        return (CaptureDefault::None, toks);
    }

    fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        toks.first()?.check_punct("[")?;
        let (cdef, mut toks) = Self::get_capture_default(&toks[1..]);
        let fst = toks.first().expect("TODO: msg");
        if fst.tok.is_punct("]") {
            return Some((Self { cdef, captures: Vec::new() }, &toks[1..]));
        } else if cdef != CaptureDefault::None {
            if !fst.tok.is_punct(",") {
                panic!("Error in Captures::parse: expected ']' or ',' after capture default");
            } else {
                toks = &toks[1..];
            }
        }
        let (captures, toks) = parse_sep_elt_list_nosep(toks, ctx, Capture::parse, parse_sep_comma).unwrap();
        toks.first().expect("TODO: msg").check_punct("]").expect("TODO: msg is not ]");
        // TODO: Scan errors instead of panicing
        captures.iter().for_each(|x| { x.check_default(cdef); });
        Some((Self { cdef, captures }, &toks[1..]))
    }
}

// Template ::= '<' TParams '>' Opt<TRequires>
#[derive(Clone, PartialEq, Debug)]
pub struct LambdaTemplate {
    pub tparams: (),
    pub trequires: Option<()>,
}

impl LambdaTemplate {
    fn parse<'a>(_toks: &'a [Token], _ctx: &mut Context) -> Option<(Self, &'a [Token])> {
       None
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum LambdaSpec {
    /// Allows body to modify the objects captured by copy and to call their non const member functions.
    /// * Cannot be used if an explicit object parameter is present. *(since C++23)*
    Mutable,
    /// Explicitly specifies that `operator()` is a constexpr function.
    /// * If `operator()` satisfy all constexpr function requirements, `operator()` will
    ///   be constexpr even if constexpr is not present
    Constexpr,
    /// Specifies that `operator()` is an immediate function.
    /// * consteval and constexpr cannot be specified at the same time
    Consteval,
    /// Specifies that `operator()` is a static member function.
    /// * static and mutable cannot be specified at the same time.
    /// * Cannot be used if captures is not empty, or an explicit object parameter is
    ///   present
    Static,
}

impl LambdaSpec {
    fn parse<'a>(toks: &'a [Token], _ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        Some((if fst.tok.is_kw("mutable") {
            Self::Mutable
        } else if fst.tok.is_kw("constexpr") {
            Self::Constexpr
        } else if fst.tok.is_kw("consteval") {
            Self::Consteval
        } else if fst.tok.is_kw("static") {
            Self::Static
        } else {
            return None;
        }, &toks[1..]))
    }

    // TODO: allowed only once, check above restrictions too
    fn _check_seq(_specs: &[Self]) -> bool { unimplemented!() }
}

// End ::= '(' SepList<Parameter, ','> ')' List<Specs> Opt<Exception> Opt<Attr> Opt<TrailingRetType> Opt<RequiresClause> CompoundStmt
//       | Opt<TrailingRetType> CompoundStmt
//       | Exception Opt<Attr> Opt<TrailingRetType> CompoundStmt
//       | NonEmptyList<Specs> Opt<Exception> Opt<Attr> Opt<TrailingRetType> CompoundStmt
//       ;
#[derive(Clone, PartialEq, Debug)]
pub struct LambdaEnd {
    pub params: Vec<ParamDecl>,
    pub specs: Vec<LambdaSpec>,
    pub noexcept: Option<Noexcept>,
    pub attr: Option<Attr>,
    pub ret_ty: Type,
    pub require: Option<()>,
    pub body: Box<Stmt>,
}

impl LambdaEnd {
    fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let (toks, params, can_have_require, can_have_end_attr) = if toks.first()?.tok.is_punct("(") {
            let (params, toks) = parse_sep_elt_list_nosep(&toks[1..], ctx, ParamDecl::parse, parse_sep_comma)?;
            toks.first().expect("TODO: msg").check_punct(")").expect("TODO: msg");
            (&toks[1..], params, true, true)
        } else {
            (toks, Vec::new(), false, false)
        };
        let (specs, toks) = parse_elt_list(toks, ctx , LambdaSpec::parse)?;
        let (noexcept, toks) = parse_opt(toks, ctx, Noexcept::parse);
        let (attr, toks) = if !specs.is_empty() || noexcept.is_some() || can_have_end_attr {
            parse_opt(toks, ctx, Attr::parse)
        } else {
            (None, toks)
        };
        let mut fst = toks.first().expect("expected '{' or '->' at end of lambda");
        let (toks, ret_ty) = if fst.tok.is_punct("->") {
            let (t, toks) = Type::parse(&toks[1..], ctx).expect("expected type after '->'");
            fst = toks.first().expect("expected function body at end of lambda");
            (toks, t)
        } else {
            (toks, Type::void())
        };
        let (require, toks) = if can_have_require {
            (None, toks)
            // parse_opt(toks, ctx, ...)
        } else {
            (None, toks)
        };
        let (body, toks) = Stmt::parse_compound(toks, ctx).expect("expected function body at end of lambda");
        Some((Self {
            params,
            specs,
            noexcept,
            attr,
            ret_ty,
            require,
            body: Box::new(body),
        }, toks))
    }
}

// LambdaExpression ::= Captures Opt<Template> Opt<Attr> End
#[derive(Clone, PartialEq, Debug)]
pub struct LambdaExpr {
    pub captures: Captures,
    pub template: Option<LambdaTemplate>,
    pub attr: Option<Attr>,
    pub end: LambdaEnd,
    pub loc: Location,
}

impl LambdaExpr {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst_loc = &toks.first()?.loc;
        let (captures, toks) = Captures::parse(toks, ctx)?;
        let (template, toks) = parse_opt(toks, ctx, LambdaTemplate::parse);
        let (attr, toks) = parse_opt(toks, ctx, Attr::parse);
        let (end, toks) = LambdaEnd::parse(toks, ctx).expect("TODO: msg");
        let last_loc = &end.body.loc;
        Some((Self {
            captures,
            template,
            attr,
            loc: Location::from_merged(fst_loc, last_loc),
            end,
        }, toks))
    }
}
