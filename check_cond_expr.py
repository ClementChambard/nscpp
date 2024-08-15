from integral_const_expr import op_eval
from toks import TOK_IDENT, TOK_LITTERAL, TOK_PUNCTUATOR, TOK_KEYWORD
from toks import LIT_STRING, LIT_STRING32
from pp_toks import PPT_IDENT, PPT_OP_PUNCT, PPT_NUMBER
from macros import is_defined, macro_expand
from phase_7 import translate_tokens

COND_DEFINE = 0
COND_NOT_DEFINE = 1
COND_EXPR = 2


def _def_or_ndef(define: int, defines: list, pp_tokens: [(int, str)]):
    if define == COND_DEFINE:
        tok = pp_tokens[0]
        if tok[0] != PPT_IDENT:
            print("#ifdef: Macro name must be an identifier")
            exit(1)
        return is_defined(defines, tok[1])
    if define == COND_NOT_DEFINE:
        tok = pp_tokens[0]
        if tok[0] != PPT_IDENT:
            print("#ifndef: Macro name must be an identifier")
            exit(1)
        return not is_defined(defines, tok[1])


# TODO: __has_include and __has_cpp_attribute
def _check_macros(defines: list, pp_tokens: [(int, str)]) -> [(int, str)]:
    after_defined = []
    in_define = 0
    for ty, v in pp_tokens:
        if in_define == 1:
            if ty == PPT_IDENT:
                after_defined.append((PPT_NUMBER, str(int(is_defined(defines, v)))))
                in_define = 0
                continue
            if ty == PPT_OP_PUNCT and v == "(":
                in_define = 2
                continue
            print("missing '(' or IDENT in defined expression")
            exit(1)
        if in_define == 2:
            if ty == PPT_IDENT:
                after_defined.append(
                    (PPT_NUMBER, str(int(is_defined(defines, v)))))
                in_define = 3
                continue
            print("missing IDENT in defined() expression")
            exit(1)
        if in_define == 3:
            if ty == PPT_OP_PUNCT and v == ")":
                in_define = 0
                continue
            print("missing ')' in defined() expression")
            exit(1)
        if ty == PPT_IDENT and v == "defined":
            in_define = 1
            continue
        after_defined.append((ty, v))
    return macro_expand(defines, after_defined)


def check_cond_expr(define: int, defines: list, pp_tokens: [(int, str)]) -> bool:
    if define in [COND_DEFINE, COND_NOT_DEFINE]:
        return _def_or_ndef(define, defines, pp_tokens)
    after_define = _check_macros(defines, pp_tokens)
    expr_tokens = translate_tokens(after_define)
    if len(expr_tokens) == 0:
        print("empty expression in #if")
        exit(1)
    tokens = []
    for t in expr_tokens:
        if t[0] == TOK_LITTERAL:
            if t[1][0] >= LIT_STRING and t[1][0] <= LIT_STRING32:
                print("Invalid token in #if expression")
                exit(1)
            # TODO: Check for char
            tokens.append(t[1][1])
        elif t[0] == TOK_IDENT or t[0] == TOK_KEYWORD:
            tokens.append(0)
        elif t[0] == TOK_PUNCTUATOR:
            tokens.append(t[1])
        else:
            print("Invalid token in #if expression")
            exit(1)
    return op_eval(tokens)




