from file_open import open_file
from phase_4 import phase_4
from phase_5_6 import phase_5_6
from phase_7 import phase_7
from toks import *
from print_ast import print_ast

pp_tokens = open_file("test.cpp")

tokens = phase_7(phase_5_6(phase_4(pp_tokens)))


def print_tok(t):
    ty = t[0]
    val = t[1]
    if ty == TOK_IDENT:
        print("IDENT('{}')".format(val))
    elif ty == TOK_LITTERAL:
        print("LITTERAL_{}('{}')".format(get_litteral_name(val[0]), val[1]))
    elif ty == TOK_KEYWORD:
        print("KEYWORD('{}')".format(val))
    elif ty == TOK_PUNCTUATOR:
        print("PUNCT('{}')".format(val))
    else:
        print(t)


def print_tok_2(t):
    ty = t[0]
    val = t[1]
    if ty == TOK_IDENT or ty == TOK_KEYWORD or ty == TOK_PUNCTUATOR:
        print(val, "", end='')
        return
    lit_ty, lit = val
    if lit_ty == LIT_BOOL:
        if lit != 0:
            print("true ", end='')
        else:
            print("false ", end='')
        return
    if lit_ty == LIT_INT:
        print(f"{lit} ", end='')
        return
    if lit_ty == LIT_UNSIGNED_INT:
        print(f"{lit}u ", end='')
        return
    if lit_ty == LIT_LONG:
        print(f"{lit}l ", end='')
        return
    if lit_ty == LIT_UNSIGNED_LONG:
        print(f"{lit}ul ", end='')
        return
    if lit_ty == LIT_LONGLONG:
        print(f"{lit}ll ", end='')
        return
    if lit_ty == LIT_UNSIGNED_LONGLONG:
        print(f"{lit}ull ", end='')
        return
    if lit_ty == LIT_SIZE:
        print(f"{lit}z ", end='')
        return
    if lit_ty == LIT_UNSIGNED_SIZE:
        print(f"{lit}uz ", end='')
        return
    if lit_ty == LIT_FLOAT:
        print(f"{lit}f ", end='')
        return
    if lit_ty == LIT_DOUBLE:
        print(f"{lit} ", end='')
        return
    if lit_ty == LIT_LONGDOUBLE:
        print(f"{lit}l ", end='')
        return
    if lit_ty == LIT_FLOAT16:
        print(f"{lit}f16 ", end='')
        return
    if lit_ty == LIT_FLOAT32:
        print(f"{lit}f32 ", end='')
        return
    if lit_ty == LIT_FLOAT164:
        print(f"{lit}f64 ", end='')
        return
    if lit_ty == LIT_FLOAT128:
        print(f"{lit}f128 ", end='')
        return
    if lit_ty == LIT_BFLOAT16:
        print(f"{lit}bf16 ", end='')
        return
    if lit_ty == LIT_CHAR:
        print(f"'{lit}' ", end='')
        return
    if lit_ty == LIT_WCHAR:
        print(f"L'{lit}' ", end='')
        return
    if lit_ty == LIT_CHAR8:
        print(f"u8'{lit}' ", end='')
        return
    if lit_ty == LIT_CHAR16:
        print(f"u'{lit}' ", end='')
        return
    if lit_ty == LIT_CHAR32:
        print(f"U'{lit}' ", end='')
        return
    if lit_ty == LIT_STRING:
        print(f'"{lit}" ', end='')
        return
    if lit_ty == LIT_WSTRING:
        print(f'L"{lit}" ', end='')
        return
    if lit_ty == LIT_STRING8:
        print(f'u8"{lit}" ', end='')
        return
    if lit_ty == LIT_STRING16:
        print(f'u"{lit}" ', end='')
        return
    if lit_ty == LIT_STRING32:
        print(f'U"{lit}" ', end='')
        return
    if lit_ty == LIT_NULLPTR:
        print("nullptr ", end='')
        return


print_ast(tokens)

#for t in tokens:
#print_tok_2(t)
