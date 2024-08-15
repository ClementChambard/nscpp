from pp_toks import *
from toks import *

# String litteral conversion and concatenation



def resolve_raw_string(s: str) -> str:
    s, ty = string_split_prefix(s)
    if ty.endswith("R"):
        ty = ty[:-1]
        fst_par = -1
        lst_par = -1
        for i in range(len(s)):
            if fst_par == -1 and s[i] == "(":
                fst_par = i
            if s[i] == ")":
                lst_par = i
        s_out = s[fst_par+1:lst_par]
    else:
        s_out = s
    return f"{ty}\"{s_out}\""


def try_concat_strings(s1: str, s2: str) -> str:
    s1, ty1 = string_split_prefix(s1)
    s2, ty2 = string_split_prefix(s2)
    ty = ""
    if ty1 == "":
        ty = ty2
    elif ty2 == "":
        ty = ty1
    elif ty2 == ty1:
        ty = ty1
    else:
        print("incompatible string types for concatenation")
        exit(1)
    s = s1 + s2
    return f'{ty}"{s}"'


def phase_5_6(pp_tokens: [(int, str)]) -> [(int, tuple)]:
    output = []
    for ty, v in pp_tokens:
        if ty == PPT_STRING:
            v = resolve_raw_string(v)
            if len(output) == 0 or output[-1][0] != PPT_STRING:
                output.append((ty, v))
                continue
            output[-1] = (ty, try_concat_strings(output[-1][1], v))
        elif ty == PPT_WHITESPACE or ty == PPT_WHITESPACE_NL:
            pass
        else:
            output.append((ty, v))
    return output
