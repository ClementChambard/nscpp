from pp_toks import PPT_OP_PUNCT, PPT_IDENT, PPT_STRING, PPT_CHAR, PPT_NUMBER, PPT_WHITESPACE, is_valid_pp_tok


def stringify(tokens: [(int, str)]) -> (int, str):
    contents = ""
    for t in tokens:
        if t[0] == PPT_WHITESPACE:
            contents += " "
        else:
            contents += t[1]
    return (PPT_STRING, '"' + contents.strip() + '"')


def is_defined(defines: list, mname: str) -> bool:
    for m in defines:
        if m[0] == mname:
            return True
    return False


def find_macro(defines: list, mname: str) -> ([str], bool, [(int, str)]):
    for n, macro in defines:
        if n == mname:
            return n, macro
    return None


def parse_macro_replace_list(args: [str], vararg: bool, toks: [(int, str)]):
    last_tok_sequence = []
    join_locations = []
    out = []
    while len(toks) > 0:
        t = toks[0]
        toks = toks[1:]
        if t[0] == PPT_OP_PUNCT and (t[1] == "#" or t[1] == "%:"):
            if len(last_tok_sequence) > 0:
                out.append(("tok", last_tok_sequence))
                last_tok_sequence = []
            if len(toks) == 0 or toks[0][0] != PPT_IDENT:
                print("'{}' not followed by macro parameter".format(t[1]))
                exit(1)
            if toks[0][1] == "__VA_ARGS__":
                toks = toks[1:]
                out.append(("sva", 0))
                continue
            if toks[0][1] not in args:
                print("'{}' not followed by macro parameter".format(t[1]))
                exit(1)
            t = toks[0]
            toks = toks[1:]
            i = args.index(t[1])
            out.append(("str", i))
            continue
        if t[0] == PPT_OP_PUNCT and (t[1] == "##" or t[1] == "%:%:"):
            if len(last_tok_sequence) > 0:
                out.append(("tok", last_tok_sequence))
                last_tok_sequence = []
            if len(toks) == 0:
                print("'{}' can't be at the end of macro".format(t[1]))
            join_locations.append(len(out))
            continue
        if t[0] == PPT_IDENT and t[1] in args:
            if len(last_tok_sequence) > 0:
                out.append(("tok", last_tok_sequence))
                last_tok_sequence = []
            i = args.index(t[1])
            out.append(("arg", i))
            continue
        if t[0] == PPT_IDENT and t[1] == "__VA_ARGS__" and vararg:
            if len(last_tok_sequence) > 0:
                out.append(("tok", last_tok_sequence))
                last_tok_sequence = []
            out.append(("vaa", 0))
            continue
        if t[0] == PPT_IDENT and t[1] == "__VA_OPT__" and vararg:
            if len(last_tok_sequence) > 0:
                out.append(("tok", last_tok_sequence))
                last_tok_sequence = []
            
            if len(toks) == 0 or toks[0][0] != PPT_OP_PUNCT or toks[0][1] != '(':
                print("missing '(' after __VA_OPT__")
                exit(1)
            toks = toks[1:]
            rep = []
            par = 1
            while len(toks) > 0:
                if toks[0][0] == PPT_OP_PUNCT and toks[0][1] == '(':
                    par += 1
                if toks[0][0] == PPT_OP_PUNCT and toks[0][1] == ')':
                    par -= 1
                    if par == 0:
                        break
                rep.append(toks[0])
                toks = toks[1:]
            if len(toks) == 0 or toks[0][0] != PPT_OP_PUNCT or toks[0][1] != ')':
                print("missing ')' in __VA_OPT__")
                exit(1)
            toks = toks[1:]
            out.append(("vao", rep))
            continue
        last_tok_sequence.append(t)
    if len(last_tok_sequence) > 0:
        out.append(("tok", last_tok_sequence))

    return (len(args), vararg, out, join_locations)


def parse_macro_arg_list(toks: [(int, str)]) -> (list, bool, [(int, str)]):
    if toks[0][0] != PPT_OP_PUNCT or toks[0][1] != "(":
        return None, False, toks, []
    toks = toks[1:]
    args = []
    vararg = False
    if len(toks) > 0 and toks[0][0] == PPT_OP_PUNCT and toks[0][1] == ")":
        toks = toks[1:]
        return 0, False, [("tok", toks)], []
    while len(toks) > 0:
        if toks[0][0] == PPT_OP_PUNCT and toks[0][1] == "...":
            toks = toks[1:]
            vararg = True
            break
        if toks[0][0] != PPT_IDENT:
            print("Invalid token in #define argument list")
            exit(1)
        args.append(toks[0][1])
        toks = toks[1:]
        if len(toks) > 0 and toks[0][0] == PPT_OP_PUNCT and toks[0][1] == ",":
            toks = toks[1:]
            continue
        break
    if len(toks) < 1 or not (toks[0][0] == PPT_OP_PUNCT and toks[0][1] == ")"):
        print("Expected token ')'")
        exit(1)
    toks = toks[1:]
    return parse_macro_replace_list(args, vararg, toks)


def exec_replace(replace: list, joins: [int], narg: int, args: [[(int, str)]], defines, excluded):
    if len(joins) > 0:
        print("## operator not supported yet")
        return []
    # For the '##' operator: revert previous change where PP Tokens are parsed immediately,
    #  then concatenate fst non spacetoken of replace group to previous non space token, (except punctuation)
    #  CHECK IT
    va = []
    if len(args) != narg:
        lva = args[narg:]
        va = lva[0]
        lva = lva[1:]
        for l in lva:
            va += [(PPT_OP_PUNCT, ",")] + l

    out = []
    for r in replace:
        if r[0] == "tok":
            out += r[1]
        elif r[0] == "str":
            out.append(stringify(args[r[1]]))
        elif r[0] == "arg":
            out += args[r[1]]
        elif r[0] == "vaa":
            out += va
        elif r[0] == "vao":
            if len(va) > 0:
                out += r[1]
        elif r[0] == "sva":
            out.append(stringify(va))
        else:
            print("Error in macro format")
            exit(1)
    return macro_expand(defines, out, excluded)


def get_macro_call_arg(pp_tokens, defines, excluded):
    arg_toks = []
    par_stack = 1
    while len(pp_tokens) > 0:
        if pp_tokens[0][0] == PPT_OP_PUNCT and pp_tokens[0][1] == "," and par_stack == 1:
            break
        if pp_tokens[0][0] == PPT_OP_PUNCT and pp_tokens[0][1] == ")":
            par_stack -= 1
            if par_stack == 0:
                break
        if pp_tokens[0][0] == PPT_OP_PUNCT and pp_tokens[0][1] == "(":
            par_stack += 1
        arg_toks.append(pp_tokens[0])
        pp_tokens = pp_tokens[1:]
    return pp_tokens, arg_toks


def get_macro_call_args(pp_tokens, count, va, defines, excluded):
    pp_tokens = pp_tokens[1:]
    min_count = count
    max_count = 1000 if va else count
    args = []
    if len(pp_tokens) == 0:
        return None
    if pp_tokens[0][0] == PPT_OP_PUNCT and pp_tokens[0][1] == ')':
        if min_count == 0:
            return args, pp_tokens[1:]
        else:
            return None
    while len(pp_tokens) > 0:
        pp_tokens, arg = get_macro_call_arg(pp_tokens, defines, excluded)
        args.append(arg)
        if len(pp_tokens) == 0 or not (pp_tokens[0][0] == PPT_OP_PUNCT and pp_tokens[0][1] == ','):
            break
        pp_tokens = pp_tokens[1:]

    if len(pp_tokens) == 0 or not (pp_tokens[0][0] == PPT_OP_PUNCT and pp_tokens[0][1] == ')'):
        return None
    pp_tokens = pp_tokens[1:]

    if len(args) >= min_count and len(args) <= max_count:
        return args, pp_tokens
    return None


def macro_expand(defines: list, pp_tokens: [(int, str)], excluded: [str] = []) -> [(int, str)]:
    out_tokens = []
    while len(pp_tokens) > 0:
        tok = pp_tokens[0]
        pp_tokens = pp_tokens[1:]
        if tok[0] != PPT_IDENT or tok[1] in excluded:
            out_tokens.append(tok)
            continue
        if tok[0] == "__FILE__":
            out_tokens.append((PPT_STRING, "\"__FILE__ (todo)\""))
            continue
        if tok[0] == "__LINE__":
            out_tokens.append((PPT_STRING, "\"__LINE__ (todo)\""))
            continue
        macro = find_macro(defines, tok[1])
        if macro is None:
            out_tokens.append(tok)
            continue
        _, macro = macro
        if macro is None:
            out_tokens += macro_expand(defines, replace, excluded + [tok[1]])
            continue
        narg, vararg, replace, joins = macro
        if narg is None:
            out_tokens += macro_expand(defines, replace, excluded + [tok[1]])
            continue
        if len(pp_tokens) == 0 or not (pp_tokens[0][0] == PPT_OP_PUNCT and pp_tokens[0][1] == "("):
            out_tokens.append(tok)
            continue
        call_args = get_macro_call_args(pp_tokens, narg, vararg, defines, excluded + [tok[1]])
        if call_args is None:
            out_tokens.append(tok)
            continue
        call_args, pp_tokens = call_args
        out_tokens += exec_replace(replace, joins, narg,
                                   call_args, defines, excluded + [tok[1]])

    return out_tokens
