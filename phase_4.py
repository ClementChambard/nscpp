from pp_toks import *
from integral_const_expr import *
from pp_directives import *
from macros import macro_expand


def cut_pp_directive(pp_tokens: [(int, str)]) -> ([(int, str)], [(int, str)]):
    pp_tokens = pp_tokens[1:]
    toks = []
    skip = 0
    for ty, v in pp_tokens:
        skip += 1
        if ty == PPT_WHITESPACE:
            continue
        if ty == PPT_WHITESPACE_NL:
            break
        toks.append((ty, v))
    pp_tokens = pp_tokens[skip:]
    return (pp_tokens, toks)



def run_pp_directive(pp_tokens: [(int, str)], defines: list, cond_stack: [bool]) -> ([(int, str)], list, [bool]):
    pp_tokens, toks = cut_pp_directive(pp_tokens)
    directive_ty, directive = toks[0]
    toks = toks[1:]
    if directive_ty != PPT_IDENT:
        return (pp_tokens, defines, cond_stack)

    # Always check these directives
    if directive == "endif":
        return pp_directive_endif(toks, defines, cond_stack, pp_tokens)
    if directive == "else":
        return pp_directive_else(toks, defines, cond_stack, pp_tokens)
    if directive == "elif":
        return pp_directive_elif(COND_EXPR, toks, defines, cond_stack, pp_tokens)
    if directive == "elifdef":
        return pp_directive_elif(COND_DEFINE, toks, defines, cond_stack, pp_tokens)
    if directive == "elifndef":
        return pp_directive_elif(COND_NOT_DEFINE, toks, defines, cond_stack, pp_tokens)

    # Check if code block is active
    if not cond_stack[-1]:
        return (pp_tokens, defines, cond_stack)

    # Only check these directives if code block active
    if directive == "if":
        return pp_directive_if(COND_EXPR, toks, defines, cond_stack, pp_tokens)
    if directive == "ifdef":
        return pp_directive_if(COND_DEFINE, toks, defines, cond_stack, pp_tokens)
    if directive == "ifndef":
        return pp_directive_if(COND_NOT_DEFINE, toks, defines, cond_stack, pp_tokens)
    if directive == "error":
        return pp_directive_error(toks, defines, cond_stack, pp_tokens)
    if directive == "warning":
        return pp_directive_warning(toks, defines, cond_stack, pp_tokens)
    if directive == "define":
        return pp_directive_define(toks, defines, cond_stack, pp_tokens)
    if directive == "undef":
        return pp_directive_undef(toks, defines, cond_stack, pp_tokens)
    if directive == "include":
        return pp_directive_include(toks, defines, cond_stack, pp_tokens)
    # TODO: #line, #file

    return (pp_tokens, defines, cond_stack)


def preprocess_till_newline(pp_tokens: [(int, str)], defines: list) -> ([(int, str)], [(int, str)]):
    out = []
    while len(pp_tokens) > 0:
        out.append(pp_tokens[0])
        pp_tokens = pp_tokens[1:]
        if out[-1][0] == PPT_WHITESPACE_NL:
            break
    return (pp_tokens, macro_expand(defines, out))


def get_predefined_macros() -> list:
    return [
        ("__cplusplus", (None, False, [(PPT_NUMBER, "202302l")], [])),
        ("__STDC_HOSTED__", (None, False, [(PPT_NUMBER, "1")], [])),
        ("__FILE__", (None, False, [(PPT_STRING, "\"__FILE__\"")], [])),
        ("__LINE__", (None, False, [(PPT_NUMBER, "1")], [])),
        # SPECIAL: __DATE__ ("Mmm dd yyyy")
        # SPECIAL: __TIME__ ("hh:mm:ss")
        ("__STDCPP_DEFAULT_NEW_ALIGNMENT__", (None, False, [(PPT_NUMBER, "1zu")], [])),
        # Extended floating point types
    ]
   


# Run preprocessor
def phase_4(pp_tokens: [(int, str)]) -> [(int, str)]:
    defines = [] + get_predefined_macros()
    out = []
    cond_stack = [True]
    while len(pp_tokens) > 0:
        if pp_tokens[0][0] == PPT_WHITESPACE:
            pp_tokens = pp_tokens[1:]
            continue
        if pp_tokens[0][0] == PPT_OP_PUNCT and (pp_tokens[0][1] == '#' or pp_tokens[0][1] == '%:'):
            pp_tokens, defines, cond_stack = run_pp_directive(
                pp_tokens, defines, cond_stack)
            continue
        if cond_stack[-1]:
            pp_tokens, o = preprocess_till_newline(pp_tokens, defines)
            out += o
        else:
            while len(pp_tokens) > 0:
                if pp_tokens[0][0] == PPT_WHITESPACE_NL:
                    pp_tokens = pp_tokens[1:]
                    break
                pp_tokens = pp_tokens[1:]
    if len(cond_stack) != 1:
        print("Error: missing #endif at end of file")
        exit(1)
    return out

