def print_w_depth(depth, *args, **kwargs):
    print(" " * depth, end="")
    print(*args, **kwargs)


def qualified_ident_str(i):
    g, l = i
    s = ""
    if g:
        s = "::"
    for i, id in enumerate(l):
        if i != 0:
            s += "::"
        s += id
    return s


def type_str(s):
    ty, d, c, v = s
    o = ""
    if ty == "base":
        o += d
    elif ty == "ptr":
        o += type_str(d) + " *"
    if c:
        o += " const"
    if v:
        o += " volatile"
    return o
        

def print_namespace_decl(decl, depth):
    print_w_depth(depth, "namespace decl:", qualified_ident_str(decl[0]))
    depth += 2
    for d in decl[1]:
        print_decl(d, depth)


def fun_type_str(r, p):
    s = type_str(r) + "("
    for i, pp in enumerate(p):
        t, _ = pp
        if i != 0:
            s += ", "
        s += type_str(t)
    return s + ")"


def print_statement(decl, depth):
    ty, dat = decl
    if ty == "compound":
        print_w_depth(depth, "compound statement:")
        depth += 2
        for s in dat:
            print_statement(s, depth)
    elif ty == "return":
        print_w_depth(depth, "return statement:")
        depth += 2
        print_w_depth(depth, dat)
    elif ty == "null":
        print_w_depth(depth, "null statement:")
    elif ty == "expr":
        print_w_depth(depth, "expr statement:")
        depth += 2
        print_w_depth(depth, dat)
    elif ty == "if":
        print_w_depth(depth, "if statement:")
        depth += 2
        cond, st1, st2 = dat
        print_w_depth(depth, cond)
        print_statement(st1, depth)
        if st2 is not None:
            print_statement(st2, depth)


def print_param_decl(decl, depth):
    t, n = decl
    s = ""
    if n is not None:
        s += "'" + n + "' "
    s += type_str(t)
    print_w_depth(depth, "param decl:", s)


def print_function_decl(decl, depth):
    r, n, p, b = decl
    print_w_depth(depth, "function decl:", n, fun_type_str(r, p))
    depth += 2
    for pp in p:
        print_param_decl(pp, depth)
    if b is not None:
        print_w_depth(depth, "compound statement:")
        depth += 2
        for s in b:
            print_statement(s, depth)


def print_decl(decl, depth):
    ty, d = decl
    if ty == "namespace":
        print_namespace_decl(d, depth)
    if ty == "function":
        print_function_decl(d, depth)

def print_ast(tu):
    depth = 0
    print("TranslationUnitDecl")
    depth += 2
    for decl in tu:
        print_decl(decl, depth)
