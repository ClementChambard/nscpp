from toks import TOK_KEYWORD, TOK_LITTERAL, TOK_IDENT, TOK_PUNCTUATOR

def parse(a):
    toks, tu = TranslationUnit(a)
    if len(toks) > 0:
        print("ERROR: not parsed tokens at end of file")
        exit(1)
    return tu


def SepList(toks, parseF, sep_tok, stop_tok = None):
    l = []
    while len(toks) > 0:
        if stop_tok is not None:
            ty, v = stop_tok
            if toks[0][0] == ty and toks[0][1] == v:
                break
        toks, a = parseF(toks)
        l.append(a)
        ty, v = sep_tok
        if toks[0][0] == ty and toks[0][1] == v:
            toks = toks[1:]
            continue
        break
    return toks, l

def AnyTok(toks):
    return toks[1:], toks[0]

def Statement(toks):
    if next_tok_is_pnct(toks, "{"):
        toks = toks[1:]
        toks, stmts = List(toks, Statement, (TOK_PUNCTUATOR, "}"))
        if not next_tok_is_pnct(toks, "}"):
            print("missing '}' in compound statement")
            exit(1)
        toks = toks[1:]
        return toks, ("compound", stmts)
    if next_tok_is_kw(toks, "if"):
        toks = toks[1:]
        if not next_tok_is_pnct(toks, "("):
            print("missing '(' in if statement")
            exit(1)
        toks = toks[1:]
        toks, cond = List(toks, AnyTok, (TOK_PUNCTUATOR, ")"))
        if not next_tok_is_pnct(toks, ")"):
            print("missing ')' in if statement")
            exit(1)
        toks = toks[1:]
        toks, stmt = Statement(toks)
        if not next_tok_is_kw(toks, "else"):
            return toks, ("if", (cond, stmt, None))
        toks = toks[1:]
        toks, stmt2 = Statement(toks)
        return toks, ("if", (cond, stmt, stmt2))
    if next_tok_is_kw(toks, "return"):
        toks = toks[1:]
        toks, expr = List(toks, AnyTok, (TOK_PUNCTUATOR, ";"))
        if not next_tok_is_pnct(toks, ";"):
            print("missing ';' in if statement")
            exit(1)
        toks = toks[1:]
        return toks, ("return", expr)
    if next_tok_is_pnct(toks, ";"):
        toks = toks[1:]
        return toks, ("null", 0)
    toks, expr = List(toks, AnyTok, (TOK_PUNCTUATOR, ";"))
    if not next_tok_is_pnct(toks, ";"):
        print("missing ';' in if statement")
        exit(1)
    toks = toks[1:]
    return toks, ("expr", expr)
        

    pass

def List(toks, parseF, stop_tok = None):
    l = []
    while len(toks) > 0:
        if stop_tok is not None:
            ty, v = stop_tok
            if toks[0][0] == ty and toks[0][1] == v:
                break
        toks, a = parseF(toks)
        l.append(a)
    return toks, l


def next_tok_is_kw(toks, kw):
    return len(toks) > 0 and tok_is_kw(toks[0], kw)


def next_tok_is_pnct(toks, pnct):
    return len(toks) > 0 and tok_is_pnct(toks[0], pnct)


def tok_is_kw(tok, kw):
    return tok[0] == TOK_KEYWORD and tok[1] == kw


def tok_is_pnct(tok, pnct):
    return tok[0] == TOK_PUNCTUATOR and tok[1] == pnct


def get_ident(toks):
    if len(toks) == 0 or toks[0][0] != TOK_IDENT:
        return None
    return toks[0][1]


def QualifiedIdent(toks):
    glob = next_tok_is_pnct(toks, "::")
    if glob:
        toks = toks[1:]
    ident = get_ident(toks)
    if ident is None:
        return None
    toks = toks[1:]
    chain = [ident]
    while len(toks) > 0 and next_tok_is_pnct(toks, "::"):
        toks = toks[1:]
        ident = get_ident(toks)
        if ident is None:
            return None
        chain.append(ident)
        toks = toks[1:]
    return toks, (glob, chain)


def NamespaceDecl(toks):
    # namespace
    toks = toks[1:]
    # ident
    ident = QualifiedIdent(toks)
    if ident is None:
        print("expected identifier after namespace")
        exit(1)
    toks, nsname = ident
    if not next_tok_is_pnct(toks, "{"):
        print("expected '{' in namespace declaration")
        exit(1)
    toks = toks[1:]
    decls = []
    while len(toks) > 0:
        if next_tok_is_pnct(toks, "}"):
            break
        toks, d = Decl(toks)
        decls.append(d)

    if not next_tok_is_pnct(toks, "}"):
        print("expected '}' in namespace declaration")
        exit(1)
    toks = toks[1:]
    return toks, (nsname, decls)


def Type(toks):
    kws = ["void", "char", "int"]
    const = False
    volatile = False
    ty = None
    while len(toks) > 0:
        if next_tok_is_kw(toks, "const"):
            if const:
                print("already const")
            const = True
            toks = toks[1:]
            continue
        if next_tok_is_kw(toks, "volatile"):
            if volatile:
                print("already volatile")
            volatile = True
            toks = toks[1:]
            continue
        if toks[0][0] == TOK_KEYWORD and toks[0][1] in kws:
            if ty is not None:
                break
            ty = ("base", toks[0][1], False, False)
            toks = toks[1:]
            continue
        if next_tok_is_pnct(toks, "*"):
            if ty is None:
                return None
            old_ty = (ty[0], ty[1], const, volatile)
            ty = ("ptr", old_ty, False, False)
            const = False
            volatile = False
            toks = toks[1:]
            continue
        break
    ty = (ty[0], ty[1], const, volatile)
    return toks, ty


def ParamDecl(toks):
    t = Type(toks)
    if t is None:
        return None
    toks, ty = t
    ident = get_ident(toks)
    if ident is not None:
        toks = toks[1:]
    return toks, (ty, ident)


def FunctionDecl(toks, ret):
    ident = get_ident(toks)
    if ident is None:
        return None
    toks = toks[1:]
    if not next_tok_is_pnct(toks, "("):
        print("expected '(' in function declaration")
        exit(1)
    toks = toks[1:]
    params = SepList(toks, ParamDecl, (TOK_PUNCTUATOR, ","), (TOK_PUNCTUATOR, ")"))
    toks, params = params
    if not next_tok_is_pnct(toks, ")"):
        print("expected ')' in function declaration")
        exit(1)
    toks = toks[1:]
    if next_tok_is_pnct(toks, ";"):
        toks = toks[1:]
        return toks, (ret, ident, params, None)
    if not next_tok_is_pnct(toks, "{"):
        return None
    toks = toks[1:]
    stack = 1
    toks, stmt = List(toks, Statement, (TOK_PUNCTUATOR, "}"))
    if not next_tok_is_pnct(toks, "}"):
        return None
    toks = toks[1:]
    return toks, (ret, ident, params, stmt)


def Decl(toks):
    if next_tok_is_kw(toks, "namespace"):
        toks, ns = NamespaceDecl(toks)
        return toks, ("namespace", ns)
    ty = Type(toks)
    if ty is not None:
        toks, ty = ty
        toks, d = FunctionDecl(toks, ty)
        return toks, ("function", d)


def TranslationUnit(toks):
    decls = []
    while len(toks) > 0:
        toks, d = Decl(toks)
        decls.append(d)
    return toks, decls
    
