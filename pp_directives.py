from pp_toks import PPT_IDENT, PPT_HEADERNAME, PPT_OP_PUNCT, PPT_STRING
from macros import *
from file_open import open_file

from check_cond_expr import check_cond_expr, COND_DEFINE, COND_NOT_DEFINE, COND_EXPR

def pp_directive_endif(toks: [(int, str)], defines: list, cond_stack: [bool], pp_tokens: [(int, str)]):
    cond_stack = cond_stack[:-1]
    if len(cond_stack) == 0:
        print("missplaced #endif")
        exit(1)
    return (pp_tokens, defines, cond_stack)


def pp_directive_else(toks: [(int, str)], defines: list, cond_stack: [bool], pp_tokens: [(int, str)]):
    if len(cond_stack) == 1:
        print("missplaced #else")
        exit(1)
    cond_stack[-1] = not cond_stack[-1]
    return (pp_tokens, defines, cond_stack)


def pp_directive_elif(define: int, toks: [(int, str)], defines: list, cond_stack: [bool], pp_tokens: [(int, str)]):
    if len(cond_stack) == 1:
        print("missplaced #elif")
        exit(1)
    if cond_stack[-1]:
        cond_stack[-1] = False
    else:
        cond_stack[-1] = check_cond_expr(define, defines, toks)
    return (pp_tokens, defines, cond_stack)


def pp_directive_if(define: int, toks: [(int, str)], defines: list, cond_stack: [bool], pp_tokens: [(int, str)]):
    cond_stack.append(check_cond_expr(define, defines, toks))
    return (pp_tokens, defines, cond_stack)


def pp_directive_error(toks: [(int, str)], defines: list, cond_stack: [bool], pp_tokens: [(int, str)]):
    print("error:")
    print(toks)
    exit(1)


def pp_directive_warning(toks: [(int, str)], defines: list, cond_stack: [bool], pp_tokens: [(int, str)]):
    print("warning:")
    print(toks)
    return (pp_tokens, defines, cond_stack)


def pp_directive_define(toks: [(int, str)], defines: list, cond_stack: [bool], pp_tokens: [(int, str)]):
    if len(toks) < 1 or toks[0][0] != PPT_IDENT:
        print("#define directive should take an identifier")
        exit(1)
    name = toks[0][1]
    toks = toks[1:]
    if len(toks) < 1:
        defines.append((name, None))
        return (pp_tokens, defines, cond_stack)
    defines.append((name, parse_macro_arg_list(toks)))
    return (pp_tokens, defines, cond_stack)


def pp_directive_undef(toks: [(int, str)], defines: list, cond_stack: [bool], pp_tokens: [(int, str)]):
    if len(toks) != 1 or toks[0][0] != PPT_IDENT:
        print("#undef directive should take an identifier")
        exit(1)
    id = toks[0][1]
    new_defines = []
    for d in defines:
        if d[0] != id:
            new_defines.append(d)
    return (pp_tokens, new_defines, cond_stack)


# TODO: lookup should be different based on <> or ""
def pp_directive_include(toks: [(int, str)], defines: list, cond_stack: [bool], pp_tokens: [(int, str)]):
    toks = macro_expand(defines, toks)
    if len(toks) != 1 and len(toks) != 3:
        print("Error in include directive: skipping")
        return (pp_tokens, defines, cond_stack)
    hname = toks[0]
    if len(toks) == 3:
        if toks[0][0] != PPT_OP_PUNCT or toks[0][1] != "<" or \
                toks[2][0] != PPT_OP_PUNCT or toks[2][1] != ">":
            print("Error in include directive: skipping")
            return (pp_tokens, defines, cond_stack)
        hname = toks[1]
    hname_s = ""
    if hname[0] == PPT_HEADERNAME:
        hname_s = hname[1]
    elif hname[0] == PPT_STRING:
        hname_s = hname[1]
        while not hname_s.startswith('"'):
            hname_s = hname_s[1:]
        hname_s = hname_s[1:-1]
    file_toks = open_file(hname_s)
    return (file_toks + pp_tokens, defines, cond_stack)
