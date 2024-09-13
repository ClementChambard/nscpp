use crate::{parsing::{
    ast::{
        ast_enum::{EnumKey, EnumSpecifier},
        attr::Attr,
        class::{ClassKey, ClassSpecifier},
        ident::MaybeQualifiedIdent,
        parse_util::{
            parse_non_empty_elt_list, parse_non_empty_sep_elt_list, parse_sep_comma,
            parse_sep_elt_list, sep_list_remove_sep,
        },
        typename::CVQualifiers,
        Expr, Type,
    },
    context::Context,
    LitType, TokType, Token,
}, util::{ast_printing, color}};

#[derive(Clone, PartialEq, Debug)]
pub struct AsmDecl {
    assembly: String,
    attr: Option<Attr>,
}

impl AsmDecl {
    fn parse_inner<'a>(toks: &'a [Token], _: &mut Context) -> Option<(String, &'a [Token])> {
        // C++26: BalancedTokenSeq
        // Before:
        let s = toks.first()?;
        if let TokType::Lit(LitType::String(s, _)) = &s.tok {
            Some((s.clone(), &toks[1..]))
        } else {
            None
        }
    }
    pub fn parse_with_attr<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        toks.first()?.check_kw("asm")?;
        toks[1..].first()?.check_punct("(")?;
        let (s, toks) = Self::parse_inner(&toks[2..], ctx)?;
        toks.first()?.check_punct(")")?;
        toks[1..].first()?.check_punct(";")?;
        Some((Self { assembly: s, attr }, &toks[2..]))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeAliasDecl {
    id: String,
    attr: Option<Attr>,
    ty: Type,
}

impl TypeAliasDecl {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        toks.first()?.check_kw("using")?;
        let id = toks[1..].first()?.tok.ident()?;
        let mut toks = &toks[2..];
        let mut attr = None;
        if let Some((a, t)) = Attr::parse(toks, ctx) {
            toks = t;
            attr = Some(a);
        }
        toks.first()?.check_punct("=")?;
        let (ty, toks) = Type::parse(&toks[1..], ctx)?;
        toks.first()?.check_punct(";")?;
        Some((Self { id, attr, ty }, &toks[1..]))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct NamespaceAliasDecl {
    alias: String,
    alias_to: MaybeQualifiedIdent,
}

impl NamespaceAliasDecl {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let kw = toks.first()?;
        if !kw.tok.is_kw("namespace") {
            return None;
        }
        let alias = toks[1..].first()?.tok.ident()?;
        if !toks[2..].first()?.tok.is_punct("=") {
            return None;
        }
        let (alias_to, toks) = MaybeQualifiedIdent::parse(&toks[3..], ctx)?;
        let semi = toks.first()?;
        if !semi.tok.is_punct(";") {
            return None;
        }
        Some((Self { alias, alias_to }, &toks[1..]))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UsingDecl {
    is_typename: bool,
    to_use: MaybeQualifiedIdent,
    // 2 versions ???
    // 'using' DeclaratorList ';'   ???
}

impl UsingDecl {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        toks.first()?.check_kw("using")?;
        let mut toks = &toks[1..];
        let is_typename = if toks.first()?.tok.is_kw("typename") {
            toks = &toks[1..];
            true
        } else {
            false
        };
        let (to_use, toks) = MaybeQualifiedIdent::parse(toks, ctx)?;
        toks.first()?.check_punct(";")?;
        Some((
            Self {
                is_typename,
                to_use,
            },
            &toks[1..],
        ))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct UsingDirectiveDecl {
    namespace: MaybeQualifiedIdent,
    attr: Option<Attr>,
}

impl UsingDirectiveDecl {
    pub fn parse_with_attr<'a>(
        attr: Option<Attr>,
        toks: &'a [Token],
        ctx: &mut Context,
    ) -> Option<(Self, &'a [Token])> {
        toks.first()?.check_kw("using")?;
        toks[1..].first()?.check_kw("namespace")?;
        let (namespace, toks) = MaybeQualifiedIdent::parse(&toks[2..], ctx)?;
        toks.first()?.check_punct(";")?;
        Some((Self { namespace, attr }, &toks[1..]))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UsingEnumDecl {
    enum_name: MaybeQualifiedIdent,
    // TODO:
    // 'using' 'enum' SimpleTemplateIdent ';'
}

impl UsingEnumDecl {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        toks.first()?.check_kw("using")?;
        toks[1..].first()?.check_kw("enum")?;
        let (enum_name, toks) = MaybeQualifiedIdent::parse(&toks[2..], ctx)?;
        toks.first()?.check_punct(";")?;
        Some((Self { enum_name }, &toks[1..]))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct StaticAssertDecl {
    e: Expr,
    err: Option<String>, // TODO: 'static_assert' '(' Expr ',' Expr ')' ';'
}

impl StaticAssertDecl {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        toks.first()?.check_kw("static_assert")?;
        toks[1..].first()?.check_punct("(")?;
        let (e, toks) = Expr::parse_no_comma(&toks[2..], ctx)?;
        let (err, toks) = if toks.first()?.tok.is_punct(",") {
            let t = toks[1..].first()?;
            let TokType::Lit(LitType::String(s, _)) = &t.tok else {
                return None;
            };
            (Some(s.clone()), &toks[2..])
        } else {
            (None, toks)
        };
        toks.first()?.check_punct(")")?;
        toks[1..].first()?.check_punct(";")?;
        Some((Self { e, err }, &toks[2..]))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SimpleTypeSpecifier {
    Char,
    Char8,
    Char16,
    Char32,
    WChar,
    Bool,
    Short,
    Int,
    Long,
    Signed,
    Unsigned,
    Float,
    Double,
    Void,
    Auto,
    // Decltype(DecltypeSpecifier), // TODO:
    // PackIndexing(PackIndexingSpecifier), // TODO:
    Class(MaybeQualifiedIdent),
    Enum(MaybeQualifiedIdent),
    Typealias(MaybeQualifiedIdent),
    // TODO: template
}

impl SimpleTypeSpecifier {
    pub fn parse<'a>(toks: &'a [Token], _: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        if fst.tok.is_kw("char") {
            Some((Self::Char, &toks[1..]))
        } else if fst.tok.is_kw("char8_t") {
            Some((Self::Char8, &toks[1..]))
        } else if fst.tok.is_kw("char16_t") {
            Some((Self::Char16, &toks[1..]))
        } else if fst.tok.is_kw("char32_t") {
            Some((Self::Char32, &toks[1..]))
        } else if fst.tok.is_kw("wchar_t") {
            Some((Self::WChar, &toks[1..]))
        } else if fst.tok.is_kw("bool") {
            Some((Self::Bool, &toks[1..]))
        } else if fst.tok.is_kw("short") {
            Some((Self::Short, &toks[1..]))
        } else if fst.tok.is_kw("int") {
            Some((Self::Int, &toks[1..]))
        } else if fst.tok.is_kw("long") {
            Some((Self::Long, &toks[1..]))
        } else if fst.tok.is_kw("signed") {
            Some((Self::Signed, &toks[1..]))
        } else if fst.tok.is_kw("unsigned") {
            Some((Self::Unsigned, &toks[1..]))
        } else if fst.tok.is_kw("float") {
            Some((Self::Float, &toks[1..]))
        } else if fst.tok.is_kw("double") {
            Some((Self::Double, &toks[1..]))
        } else if fst.tok.is_kw("void") {
            Some((Self::Void, &toks[1..]))
        } else if fst.tok.is_kw("auto") {
            Some((Self::Auto, &toks[1..]))
        } else if fst.tok.is_kw("decltype") {
            None
        } else {
            // TODO: packindexing, template
            // let (id, toks) = MaybeQualifiedIdent::parse(toks)?;
            // TODO: check if it is existing class/enum/typedef
            // for now, never.
            None //Some((Self::Class(id), toks))
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum TypeSpecifier {
    Const,
    Volatile,
    Typename,
    ElaboratedClass(ClassKey, MaybeQualifiedIdent), // TODO: template version
    ElaboratedEnum(EnumKey, MaybeQualifiedIdent),
    Simple(SimpleTypeSpecifier),
    Class(ClassSpecifier),
    Enum(EnumSpecifier),
}

impl TypeSpecifier {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        Some(if fst.tok.is_kw("const") {
            (Self::Const, &toks[1..])
        } else if fst.tok.is_kw("volatile") {
            (Self::Volatile, &toks[1..])
        } else if fst.tok.is_kw("typename") {
            (Self::Typename, &toks[1..])
        } else if fst.tok.is_kw("enum") {
            if let Some((e, toks)) = EnumSpecifier::parse(toks, ctx) {
                (Self::Enum(e), toks)
            } else {
                let (key, toks) = EnumKey::parse(toks, ctx)?;
                let (id, toks) = MaybeQualifiedIdent::parse(toks, ctx)?;
                (Self::ElaboratedEnum(key, id), toks)
            }
        } else if fst.tok.is_kw("class") || fst.tok.is_kw("struct") || fst.tok.is_kw("union") {
            if let Some((c, toks)) = ClassSpecifier::parse(toks, ctx) {
                (Self::Class(c), toks)
            } else {
                let (key, toks) = ClassKey::parse(toks, ctx)?;
                let (id, toks) = MaybeQualifiedIdent::parse(toks, ctx)?;
                (Self::ElaboratedClass(key, id), toks)
            }
        } else {
            let (simple, toks) = SimpleTypeSpecifier::parse(toks, ctx)?;
            (Self::Simple(simple), toks)
        })
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Specifier {
    Typedef,
    Inline,
    Virtual,
    Explicit,
    Friend,
    Constexpr,
    Consteval,
    Constinit,
    Static,
    ThreadLocal,
    Extern,
    Mutable,
    Type(TypeSpecifier),
    Attr(Attr),
}
impl Specifier {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        Some((
            if fst.tok.is_kw("typedef") {
                Self::Typedef
            } else if fst.tok.is_kw("inline") {
                Self::Inline
            } else if fst.tok.is_kw("virtual") {
                Self::Virtual
            } else if fst.tok.is_kw("explicit") {
                Self::Explicit
            } else if fst.tok.is_kw("friend") {
                Self::Friend
            } else if fst.tok.is_kw("constexpr") {
                Self::Constexpr
            } else if fst.tok.is_kw("consteval") {
                Self::Consteval
            } else if fst.tok.is_kw("constinit") {
                Self::Constinit
            } else if fst.tok.is_kw("static") {
                Self::Static
            } else if fst.tok.is_kw("thread_local") {
                Self::ThreadLocal
            } else if fst.tok.is_kw("extern") {
                Self::Extern
            } else if fst.tok.is_kw("mutable") {
                Self::Mutable
            } else if let Some((ts, toks)) = TypeSpecifier::parse(toks, ctx) {
                return Some((Self::Type(ts), toks));
            } else if let Some((attr, toks)) = Attr::parse(toks, ctx) {
                return Some((Self::Attr(attr), toks));
            } else {
                return None;
            },
            &toks[1..],
        ))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Initializer {
    Clauses(Vec<Initializer>),
    Design(Vec<DesignatedInitializer>),
    Expr(Expr),
}

impl Initializer {
    pub fn parse_clause<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        if toks.first()?.tok.is_punct("{") {
            if let Some((l, toks)) =
                parse_sep_elt_list(&toks[1..], ctx, Initializer::parse_clause, parse_sep_comma)
            {
                toks.first()?.check_punct("}")?;
                Some((Self::Clauses(sep_list_remove_sep(l)), &toks[1..]))
            } else {
                let (l, toks) = parse_non_empty_sep_elt_list(
                    &toks[1..],
                    ctx,
                    DesignatedInitializer::parse,
                    parse_sep_comma,
                )?;
                toks.first()?.check_punct("}")?;
                Some((Self::Design(sep_list_remove_sep(l)), &toks[1..]))
            }
        } else {
            let (e, toks) = Expr::parse_no_comma(toks, ctx)?;
            Some((Self::Expr(e), toks))
        }
    }

    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        if fst.tok.is_punct("=") {
            Self::parse_clause(&toks[1..], ctx)
        } else if fst.tok.is_punct("(") {
            let (l, toks) = parse_non_empty_sep_elt_list(
                toks,
                ctx,
                Initializer::parse_clause,
                parse_sep_comma,
            )?;
            toks.first()?.check_punct(")")?;
            Some((Self::Clauses(sep_list_remove_sep(l)), &toks[1..]))
        } else if fst.tok.is_punct("{") {
            Self::parse_clause(toks, ctx)
        } else {
            None
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct DesignatedInitializer {
    id: String,
    clause: Initializer,
}

impl DesignatedInitializer {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        toks.first()?.check_punct(".")?;
        let id = toks[1..].first()?.tok.ident()?;
        toks[2..].first()?.check_punct("=")?;
        let (clause, toks) = Initializer::parse_clause(&toks[3..], ctx)?;
        Some((Self { id, clause }, toks))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum RefQualifier {
    LValueRef,
    RValueRef,
}

#[derive(Clone, PartialEq, Debug)]
pub enum DeclaratorType {
    Id(MaybeQualifiedIdent),            // Opt<NestedNameSpecifier> id Opt<Attr>
    VA(String),                         // '...' id Opt<Attr>
    Ptr(CVQualifiers, Box<Declarator>), // '*' Opt<Attr> Opt<CVQualifier> Declarator
    // NestedNameSpecifier * Opt<Attr> Opt<CVQualifier> Declarator
    Ref(RefQualifier, Box<Declarator>), // RefQualifier Opt<Attr> Declarator
                                        // Array(Box<Declarator> /*noptr*/, Option<Expr>), // Declarator(NoPtr) '[' Opt<Expr(Constexpr)> ']' Opt<Attr>
                                        // Fun(
                                        //     Box<Declarator>, /*noptr*/ /*, Vec<Parameter>, CV, Ref, NoEx, TrailingRet*/
                                        // ), // Declarator(NoPtr) '(' SepList<Parameter, ','> ')' Opt<CVQualifier> Opt<RefQualifier> Opt<NoExcept> Opt<Attr> Opt<TrailingRetType>
                                        //              // ??? Some can be in parens ???
}

#[derive(Clone, PartialEq, Debug)]
pub struct Declarator {
    decl: DeclaratorType,
    attr: Option<Attr>,
}

impl Declarator {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let fst = toks.first()?;
        if fst.tok.is_punct("&&") || fst.tok.is_punct("&") {
            let rq = if fst.tok.is_punct("&&") {
                RefQualifier::RValueRef
            } else {
                RefQualifier::LValueRef
            };
            let (attr, toks) = if let Some((attr, toks)) = Attr::parse(&toks[1..], ctx) {
                (Some(attr), toks)
            } else {
                (None, &toks[1..])
            };
            let (decl, toks) = Declarator::parse(toks, ctx)?;
            Some((
                Self {
                    decl: DeclaratorType::Ref(rq, Box::new(decl)),
                    attr,
                },
                toks,
            ))
        } else if fst.tok.is_punct("...") {
            let id = toks[1..].first()?.tok.ident()?;
            let (attr, toks) = if let Some((attr, toks)) = Attr::parse(&toks[2..], ctx) {
                (Some(attr), toks)
            } else {
                (None, &toks[1..])
            };
            Some((
                Self {
                    decl: DeclaratorType::VA(id),
                    attr,
                },
                toks,
            ))
        } else if fst.tok.is_punct("*") {
            let (attr, toks) = if let Some((attr, toks)) = Attr::parse(&toks[1..], ctx) {
                (Some(attr), toks)
            } else {
                (None, &toks[1..])
            };
            // TODO: CV qualifiers here
            let (decl, toks) = Declarator::parse(toks, ctx)?;
            Some((
                Self {
                    decl: DeclaratorType::Ptr(CVQualifiers::None, Box::new(decl)),
                    attr,
                },
                toks,
            ))
        } else {
            // TODO: only Id for now
            let (id, mut toks) = MaybeQualifiedIdent::parse(toks, ctx)?;
            let attr = if let Some((attr, t)) = Attr::parse(toks, ctx) {
                toks = t;
                Some(attr)
            } else {
                None
            };
            Some((
                Self {
                    decl: DeclaratorType::Id(id),
                    attr,
                },
                toks,
            ))
        }
    }
}
#[derive(Clone, PartialEq, Debug)]
pub struct InitDeclarator {
    decl: Declarator,
    init: Option<Initializer>,
    // TODO: or RequiresClause
}
impl InitDeclarator {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let (decl, toks) = Declarator::parse(toks, ctx)?;
        Some(if let Some((init, toks)) = Initializer::parse(toks, ctx) {
            (
                Self {
                    decl,
                    init: Some(init),
                },
                toks,
            )
        } else {
            (Self { decl, init: None }, toks)
        })
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct SimpleDecl {
    specifiers: Vec<Specifier>,
    declarators: Vec<InitDeclarator>,
}

impl SimpleDecl {
    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        let (specifiers, toks) = parse_non_empty_elt_list(toks, ctx, Specifier::parse)?;
        let (decl, toks) = parse_sep_elt_list(toks, ctx, InitDeclarator::parse, parse_sep_comma)?;
        let declarators = sep_list_remove_sep(decl);
        toks.first()?.check_punct(";")?;
        Some((
            Self {
                specifiers,
                declarators,
            },
            &toks[1..],
        ))
    }

    pub fn print_ast(&self, _fstr: &str, _lstr: &str) -> String {
        format!("Not implemented:\n{:?}", self)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum BlockDeclarationType {
    Asm(AsmDecl),
    TypeAlias(TypeAliasDecl),
    NamespaceAlias(NamespaceAliasDecl),
    Using(UsingDecl),
    UsingDirective(UsingDirectiveDecl),
    UsingEnum(UsingEnumDecl),
    StaticAssert(StaticAssertDecl),
    Simple(SimpleDecl),
}

#[derive(Clone, PartialEq, Debug)]
pub struct BlockDeclaration {
    decl: BlockDeclarationType,
}

impl BlockDeclaration {
    fn parse_using<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        if let Some((d, toks)) = TypeAliasDecl::parse(toks, ctx) {
            Some((
                Self {
                    decl: BlockDeclarationType::TypeAlias(d),
                },
                toks,
            ))
        } else if let Some((d, toks)) = UsingDecl::parse(toks, ctx) {
            Some((
                Self {
                    decl: BlockDeclarationType::Using(d),
                },
                toks,
            ))
        } else if let Some((d, toks)) = UsingDirectiveDecl::parse_with_attr(None, toks, ctx) {
            Some((
                Self {
                    decl: BlockDeclarationType::UsingDirective(d),
                },
                toks,
            ))
        } else if let Some((d, toks)) = UsingEnumDecl::parse(toks, ctx) {
            Some((
                Self {
                    decl: BlockDeclarationType::UsingEnum(d),
                },
                toks,
            ))
        } else {
            None
        }
    }

    pub fn parse<'a>(toks: &'a [Token], ctx: &mut Context) -> Option<(Self, &'a [Token])> {
        // TODO: Location
        if let Some((attr, toks)) = Attr::parse(toks, ctx) {
            let fst = toks.first()?;
            if fst.tok.is_kw("asm") {
                let (d, toks) = AsmDecl::parse_with_attr(Some(attr), toks, ctx)?;
                return Some((
                    Self {
                        decl: BlockDeclarationType::Asm(d),
                    },
                    toks,
                ));
            } else if fst.tok.is_kw("using") {
                let (d, toks) = UsingDirectiveDecl::parse_with_attr(Some(attr), toks, ctx)?;
                return Some((
                    Self {
                        decl: BlockDeclarationType::UsingDirective(d),
                    },
                    toks,
                ));
            } else {
                return None;
            }
        }
        let fst = toks.first()?;
        if fst.tok.is_kw("using") {
            Self::parse_using(toks, ctx)
        } else if fst.tok.is_kw("asm") {
            let (d, toks) = AsmDecl::parse_with_attr(None, toks, ctx)?;
            return Some((
                Self {
                    decl: BlockDeclarationType::Asm(d),
                },
                toks,
            ));
        } else if fst.tok.is_kw("namespace") {
            let (d, toks) = NamespaceAliasDecl::parse(toks, ctx)?;
            Some((
                Self {
                    decl: BlockDeclarationType::NamespaceAlias(d),
                },
                toks,
            ))
        } else if fst.tok.is_kw("static_assert") {
            let (d, toks) = StaticAssertDecl::parse(toks, ctx)?;
            Some((
                Self {
                    decl: BlockDeclarationType::StaticAssert(d),
                },
                toks,
            ))
        } else {
            let (d, toks) = SimpleDecl::parse(toks, ctx)?;
            Some((
                Self {
                    decl: BlockDeclarationType::Simple(d),
                },
                toks,
            ))
        }
    }
}

impl ast_printing::AstPrint for BlockDeclaration {
    fn print_ast(&self, fstr: &str, lstr: &str) -> String {
        let mut out = color::format_str().s_c(fstr, color::BLUE).s("TEMP ").build();
        match &self.decl {
            BlockDeclarationType::Asm(asm) => out.push_str(&format!("AsmDecl: {}", asm.assembly)),
            BlockDeclarationType::Simple(s) => return s.print_ast(fstr, lstr),
            BlockDeclarationType::Using(u) => {
                out.push_str(&format!("Using: {} {}", u.is_typename, u.to_use.get_str()));
            }
            BlockDeclarationType::TypeAlias(a) => {
                out.push_str(&format!("using {} = {}", a.id, a.ty.ty_str()));
            }
            BlockDeclarationType::UsingEnum(e) => {
                out.push_str(&format!("using enum: {}", e.enum_name.get_str()));
            }
            BlockDeclarationType::StaticAssert(sa) => {
                out.push_str(&format!("StaticAssert: err={:?}\n", sa.err));
                out.push_str(&sa.e.print_ast(&format!("{lstr}`-"), &format!("{lstr}  ")));
            }
            BlockDeclarationType::NamespaceAlias(na) => {
                out.push_str(&format!(
                    "namespace {} = {}",
                    na.alias,
                    na.alias_to.get_str()
                ));
            }
            BlockDeclarationType::UsingDirective(un) => {
                out.push_str(&format!("using namespace {}", un.namespace.get_str()));
            }
        }
        out
    }
}

// #[cfg(test)]
// mod test {
//     use crate::{
//         file_opening::read_source,
//         parsing::{ast::decl::block_decl::BlockDeclaration, translate_tokens},
//         preprocessor::preprocess_file,
//     };
//
//     #[test]
//     fn testtest() {
//         let tok = read_source("namespace a = b::c;", "dummy");
//         let tok = preprocess_file("dummy", tok);
//         let tok = translate_tokens(tok);
//         let res = BlockDeclaration::parse(&tok);
//         println!("{:#?}", res);
//         panic!();
//     }
// }
