TOK_IDENT = 0
TOK_KEYWORD = 1
TOK_LITTERAL = 2
TOK_PUNCTUATOR = 3

LIT_BOOL = 0
LIT_INT = 1
LIT_UNSIGNED_INT = 2
LIT_LONG = 3
LIT_UNSIGNED_LONG = 4
LIT_LONGLONG = 5
LIT_UNSIGNED_LONGLONG = 6
LIT_SIZE = 7
LIT_UNSIGNED_SIZE = 8
LIT_FLOAT = 9
LIT_DOUBLE = 10
LIT_LONGDOUBLE = 11
LIT_FLOAT16 = 12
LIT_FLOAT32 = 13
LIT_FLOAT164 = 14
LIT_FLOAT128 = 15
LIT_BFLOAT16 = 16
LIT_CHAR = 17
LIT_WCHAR = 18
LIT_CHAR8 = 19
LIT_CHAR16 = 20
LIT_CHAR32 = 21
LIT_STRING = 22
LIT_WSTRING = 23
LIT_STRING8 = 24
LIT_STRING16 = 25
LIT_STRING32 = 26
LIT_NULLPTR = 27


def get_litteral_name(l: int):
    return [
        "BOOL",
        "INT",
        "UNSIGNED_INT",
        "LONG",
        "UNSIGNED_LONG",
        "LONGLONG",
        "UNSIGNED_LONGLONG",
        "SIZE",
        "UNSIGNED_SIZE",
        "FLOAT",
        "DOUBLE",
        "LONGDOUBLE",
        "FLOAT16",
        "FLOAT32",
        "FLOAT164",
        "FLOAT128",
        "BFLOAT16",
        "CHAR",
        "WCHAR",
        "CHAR8",
        "CHAR16",
        "CHAR32",
        "STRING",
        "WSTRING",
        "STRING8",
        "STRING16",
        "STRING32",
        "NULLPTR",
    ][l]


def string_split_prefix(s: str, char: str = '"') -> (str, str):
    ty = ""
    for c in s:
        if c == char:
            break
        ty += c
    s = s[len(ty)+1:-1]
    return s, ty


def actual_int_type(num: int, unsigned: bool, ty: int) -> int:
    i = 0
    while num != 0:
        num //= 2
        i += 1
    if i > 64:
        print("integer litteral too big")
        exit(1)
    min_bits = 32 if ty == LIT_INT else 64
    if not unsigned:
        min_bits -= 1
    if min_bits > i:
        return ty + int(unsigned)
    elif min_bits > 32:
        return LIT_LONGLONG + int(unsigned)
    else:
        return LIT_INT + int(unsigned)


def number_decide_type_int(num: int, suffix: str) -> (int, str):
    unsigned = False
    ty = LIT_INT
    found_digit_suffix = False
    while len(suffix) > 0:
        if suffix.startswith("u"):
            unsigned = True
            suffix = suffix[1:]
        elif suffix.startswith("ll") and not found_digit_suffix:
            ty = LIT_LONGLONG
            suffix = suffix[2:]
        elif suffix.startswith("l") and not found_digit_suffix:
            ty = LIT_LONG
            suffix = suffix[1:]
        elif suffix.startswith("z") and not found_digit_suffix:
            ty = LIT_SIZE
            suffix = suffix[1:]
        else:
            print("Error in number suffix", suffix)
            exit(1)
    return (TOK_LITTERAL, (actual_int_type(num, unsigned, ty), num))


def number_decide_type_float(num: float, suffix: str) -> (float, str):
    if suffix == "":
        ty = LIT_DOUBLE
    elif suffix == "f":
        ty = LIT_FLOAT
    elif suffix == "l":
        ty = LIT_LONGDOUBLE
    elif suffix == "f16":
        ty = LIT_FLOAT16
    elif suffix == "f32":
        ty = LIT_FLOAT32
    elif suffix == "f64":
        ty = LIT_FLOAT64
    elif suffix == "f128":
        ty = LIT_FLOAT128
    elif suffix == "bf16":
        ty = LIT_BFLOAT16
    else:
        print("Error in number suffix", suffix)
        exit(1)
    return (num, ty)


def number_parse_binary(num: str) -> (int, str):
    res = 0
    while len(num) > 0 and num[0] in "01":
        res *= 2
        res += int(num[0])
        num = num[1:]
    return number_decide_type_int(res, num)


def number_parse_float_deci(num: str) -> (float, str):
    has_exponent = "e" in num
    exponent_value = 0
    if has_exponent:
        split = num.split("e")
        if len(split) != 2 or len(split[1]) < 1:
            print("Error: in float exponent")
            exit(1)
        pre_exponent = split[0]
        post_exponent = split[1]
        exponent_sign = 1
        if post_exponent[0] == "+":
            post_exponent = post_exponent[1:]
        elif post_exponent[0] == "-":
            exponent_sign = -1
            post_exponent = post_exponent[1:]
        exponent_value = 0
        while len(post_exponent) > 0 and post_exponent[0].isdigit():
            exponent_value *= 10
            exponent_value += int(post_exponent[0])
            post_exponent = post_exponent[1:]
        num = pre_exponent + post_exponent
        exponent_value *= exponent_sign
    has_deci = "." in num
    deci_value = 0
    if has_deci:
        split = num.split(".")
        if len(split) != 2:
            print("Error: in float deci")
            exit(1)
        if len(split[0]) == 0 and not split[1][0].isdigit():
            print("Error: in float deci")
            exit(1)
        pre_split = split[0]
        post_split = split[1]
        v = 1.0 / 10.0
        while len(post_split) > 0 and post_split[0].isdigit():
            deci_value += float(post_split[0]) * v
            v /= 10.0
            post_split = post_split[1:]
        num = pre_split + post_split
    res = 0.0
    while len(num) > 0 and num[0].isdigit():
        res *= 10.0
        res += float(num[0])
        num = num[1:]
    res += deci_value
    res = res * (10 ** exponent_value)
    return number_decide_type_float(res, num)


def number_parse_float_hex(num: str) -> (float, str):
    has_exponent = "p" in num
    exponent_value = 0
    if has_exponent:
        split = num.split("p")
        print(split)
        if len(split) != 2 or len(split[1]) < 1:
            print("Error: in float exponent")
            exit(1)
        pre_exponent = split[0]
        post_exponent = split[1]
        exponent_sign = 1
        if post_exponent[0] == "+":
            post_exponent = post_exponent[1:]
        elif post_exponent[0] == "-":
            exponent_sign = -1
            post_exponent = post_exponent[1:]
        exponent_value = 0
        while len(post_exponent) > 0 and post_exponent[0].isdigit():
            exponent_value *= 10
            exponent_value += int(post_exponent[0])
            post_exponent = post_exponent[1:]
        num = pre_exponent + post_exponent
        exponent_value *= exponent_sign
    has_deci = "." in num
    deci_value = 0
    if has_deci:
        split = num.split(".")
        if len(split) != 2:
            print("Error: in float deci")
            exit(1)
        if len(split[0]) == 0 and not split[1][0] in "0123456789abcdef":
            print("Error: in float deci")
            exit(1)
        pre_split = split[0]
        post_split = split[1]
        v = 1.0 / 16.0
        while len(post_split) > 0 and post_split[0] in "0123456789abcdef":
            if post_split[0] in "abcdef":
                deci_value += float(10 + ord(post_split[0]) - ord('a')) * v
            else:
                deci_value += float(post_split[0]) * v
            v /= 16.0
            post_split = post_split[1:]
        num = pre_split + post_split
    res = 0.0
    while len(num) > 0 and num[0] in "0123456789abcdef":
        res *= 16.0
        if num[0] in "abcdef":
            res += float(10 + ord(num[0]) - ord('a'))
        else:
            res += float(num[0])
        num = num[1:]
    res += deci_value
    res = res * (10 ** exponent_value)
    return number_decide_type_float(res, num)


def number_parse_hex(num: str) -> tuple:
    if "p" in num or "." in num:
        return number_parse_float_hex(num)
    res = 0
    while len(num) > 0 and num[0] in "0123456789abcdef":
        res *= 16
        if num[0] in "abcdef":
            res += 10 + ord(num[0]) - ord('a')
        else:
            res += int(num[0])
        num = num[1:]
    return number_decide_type_int(res, num)


def number_parse_oct(num: str) -> (int, str):
    res = 0
    while len(num) > 0 and num[0] in "01234567":
        res *= 8
        res += int(num[0])
        num = num[1:]
    return number_decide_type_int(res, num)


def number_parse_deci(num: str) -> (int, str):
    res = 0
    while len(num) > 0 and num[0].isdigit():
        res *= 10
        res += int(num[0])
        num = num[1:]
    return number_decide_type_int(res, num)


def tok_parse_number(num: str) -> tuple:
    num = num.lower()
    num = num.replace("'", "")
    if num.startswith("0b"):
        return number_parse_binary(num[2:])
    if num.startswith("0x"):
        return number_parse_hex(num[2:])
    if "e" in num or "." in num:
        return number_parse_float_deci(num)
    if num.startswith("0"):
        return number_parse_oct(num)
    return number_parse_deci(num)


def tok_parse_escape(s: str) -> (str, str):
    s = s[1:]
    if s[0] in "'\"\\?":
        return s[1:], s[0]
    if s[0] == "a":
        return s[1:], "\a"
    if s[0] == "b":
        return s[1:], "\b"
    if s[0] == "f":
        return s[1:], "\f"
    if s[0] == "n":
        return s[1:], "\n"
    if s[0] == "r":
        return s[1:], "\r"
    if s[0] == "t":
        return s[1:], "\t"
    if s[0] == "v":
        return s[1:], "\v"
    if s[0] in "01234567":
        num = s[0]
        s = s[1:]
        if len(s) > 0 and s[0] in "01234567":
            num += s[0]
            s = s[1:]
        if len(s) > 0 and s[0] in "01234567":
            num += s[0]
            s = s[1:]
        o_char = 0
        while len(num) > 0:
            o_char *= 8
            o_char += int(num[0])
            num = num[1:]
        return s, chr(o_char)
    if s.startswith("o{"):
        s = s[2:]
        num = 0
        while len(s) > 0 and s[0] in "01234567":
            num *= 8
            num += int(s[0])
            s = s[1:]
        return s[1:], chr(num)
    if s[0] == "x":
        s = s[1:]
        p = False
        if s[0] == "{":
            s = s[1:]
            p = True
        num = 0
        while len(s) > 0 and s[0] in "0123456789abcdefABCDEF":
            num *= 16
            if s[0] in "abcdef":
                num += 10 + ord(s[0]) - ord('a')
            elif s[0] in "ABCDEF":
                num += 10 + ord(s[0]) - ord('A')
            else:
                num += int(s[0])
            s = s[1:]
        if p:
            s = s[1:]
        return s, chr(num)
    if s.startswith("u{"):
        s = s[2:]
        num = 0
        while len(s) > 0 and s[0] in "0123456789abcdefABCDEF":
            num *= 16
            if s[0] in "abcdef":
                num += 10 + ord(s[0]) - ord('a')
            elif s[0] in "ABCDEF":
                num += 10 + ord(s[0]) - ord('A')
            else:
                num += int(s[0])
            s = s[1:]
        return s[1:], chr(num)
    if s[0] == "u":
        n = s[1:5]
        s = s[5:]
        num = 0
        for c in n:
            num *= 16
            if c in "abcdef":
                num += 10 + ord(c) - ord('a')
            elif c in "ABCDEF":
                num += 10 + ord(c) - ord('A')
            else:
                num += int(c)
        return s, chr(num)
    if s[0] == "U":
        n = s[1:9]
        s = s[9:]
        num = 0
        for c in n:
            num *= 16
            if c in "abcdef":
                num += 10 + ord(c) - ord('a')
            elif c in "ABCDEF":
                num += 10 + ord(c) - ord('A')
            else:
                num += int(c)
        return s, chr(num)
    # TODO:
    # \N{NAME}
    return None


def find_char_lit_type(ty: str) -> int:
    if ty == "":
        return LIT_CHAR
    if ty == "L":
        return LIT_WCHAR
    if ty == "u8":
        return LIT_CHAR8
    if ty == "u":
        return LIT_CHAR16
    if ty == "U":
        return LIT_CHAR32
    print("ERROR reading char type", ty)
    exit(1)


def tok_parse_string(s: str) -> (str, str):
    s, ty = string_split_prefix(s)
    s_out = ""
    while len(s) > 0:
        if s[0] != "\\":
            s_out += s[0]
            s = s[1:]
            continue
        o = tok_parse_escape(s)
        if o is None:
            print("unsupported escape sequence")
            exit(1)
        s, e = o
        s_out += e
    return (TOK_LITTERAL, (find_char_lit_type(ty) + LIT_STRING - LIT_CHAR, s_out))


def tok_parse_char(s: str) -> (int, str):
    s, ty = string_split_prefix(s, "'")
    s_out = ""
    while len(s) > 0:
        if s[0] != "\\":
            s_out += s[0]
            s = s[1:]
            continue
        o = tok_parse_escape(s)
        if o is None:
            print("unsupported escape sequence")
            exit(1)
        s, e = o
        s_out += e
    return (TOK_LITTERAL, (find_char_lit_type(ty), s_out))


def translate_ident(ident: str):
    alt_list = [
        ["alignas", TOK_KEYWORD, "alignas"],
        ["alignof", TOK_KEYWORD, "alignof"],
        ["and", TOK_PUNCTUATOR, "&&"],
        ["and_eq", TOK_PUNCTUATOR, "&="],
        ["asm", TOK_KEYWORD, "asm"],
        ["atomic_cancel", TOK_KEYWORD, "atomic_cancel"],
        ["atomic_commit", TOK_KEYWORD, "atomic_commit"],
        ["atomic_noexcept", TOK_KEYWORD, "atomic_noexcept"],
        ["auto", TOK_KEYWORD, "auto"],
        ["bitand", TOK_PUNCTUATOR, "&"],
        ["bitor", TOK_PUNCTUATOR, "|"],
        ["bool", TOK_KEYWORD, "bool"],
        ["break", TOK_KEYWORD, "break"],
        ["case", TOK_KEYWORD, "case"],
        ["catch", TOK_KEYWORD, "catch"],
        ["char", TOK_KEYWORD, "char"],
        ["char8_t", TOK_KEYWORD, "char8_t"],
        ["char16_t", TOK_KEYWORD, "char16_t"],
        ["char32_t", TOK_KEYWORD, "char32_t"],
        ["class", TOK_KEYWORD, "class"],
        ["compl", TOK_PUNCTUATOR, "~"],
        ["concept", TOK_KEYWORD, "concept"],
        ["const", TOK_KEYWORD, "const"],
        ["consteval", TOK_KEYWORD, "consteval"],
        ["constexpr", TOK_KEYWORD, "constexpr"],
        ["constinit", TOK_KEYWORD, "constinit"],
        ["const_cast", TOK_KEYWORD, "const_cast"],
        ["continue", TOK_KEYWORD, "continue"],
        ["co_await", TOK_KEYWORD, "co_await"],
        ["co_return", TOK_KEYWORD, "co_return"],
        ["co_yield", TOK_KEYWORD, "co_yield"],
        ["decltype", TOK_KEYWORD, "decltype"],
        ["default", TOK_KEYWORD, "default"],
        ["delete", TOK_KEYWORD, "delete"],
        ["do", TOK_KEYWORD, "do"],
        ["double", TOK_KEYWORD, "double"],
        ["dynamic_cast", TOK_KEYWORD, "dynamic_cast"],
        ["else", TOK_KEYWORD, "else"],
        ["enum", TOK_KEYWORD, "enum"],
        ["explicit", TOK_KEYWORD, "explicit"],
        ["export", TOK_KEYWORD, "export"],
        ["extern", TOK_KEYWORD, "extern"],
        ["false", TOK_LITTERAL, (LIT_BOOL, 0)],
        ["float", TOK_KEYWORD, "float"],
        ["for", TOK_KEYWORD, "for"],
        ["friend", TOK_KEYWORD, "friend"],
        ["goto", TOK_KEYWORD, "goto"],
        ["if", TOK_KEYWORD, "if"],
        ["inline", TOK_KEYWORD, "inline"],
        ["int", TOK_KEYWORD, "int"],
        ["long", TOK_KEYWORD, "long"],
        ["mutable", TOK_KEYWORD, "mutable"],
        ["namespace", TOK_KEYWORD, "namespace"],
        ["new", TOK_KEYWORD, "new"],
        ["noexcept", TOK_KEYWORD, "noexcept"],
        ["not", TOK_PUNCTUATOR, "!"],
        ["not_eq", TOK_PUNCTUATOR, "!="],
        ["nullptr", TOK_LITTERAL, (LIT_NULLPTR, 0)],
        ["operator", TOK_KEYWORD, "operator"],
        ["or", TOK_PUNCTUATOR, "||"],
        ["or_eq", TOK_PUNCTUATOR, "|="],
        ["private", TOK_KEYWORD, "private"],
        ["protected", TOK_KEYWORD, "protected"],
        ["public", TOK_KEYWORD, "public"],
        ["reflexpr", TOK_KEYWORD, "reflexpr"],
        ["register", TOK_KEYWORD, "register"],
        ["reinterpret_cast", TOK_KEYWORD, "reinterpret_cast"],
        ["requires", TOK_KEYWORD, "requires"],
        ["return", TOK_KEYWORD, "return"],
        ["short", TOK_KEYWORD, "short"],
        ["signed", TOK_KEYWORD, "signed"],
        ["sizeof", TOK_KEYWORD, "sizeof"],
        ["static", TOK_KEYWORD, "static"],
        ["static_assert", TOK_KEYWORD, "static_assert"],
        ["static_cast", TOK_KEYWORD, "static_cast"],
        ["struct", TOK_KEYWORD, "struct"],
        ["switch", TOK_KEYWORD, "switch"],
        ["synchronized", TOK_KEYWORD, "synchronized"],
        ["template", TOK_KEYWORD, "template"],
        ["this", TOK_KEYWORD, "this"],
        ["thread_local", TOK_KEYWORD, "thread_local"],
        ["throw", TOK_KEYWORD, "throw"],
        ["true", TOK_LITTERAL, (LIT_BOOL, 1)],
        ["try", TOK_KEYWORD, "try"],
        ["typedef", TOK_KEYWORD, "typedef"],
        ["typeid", TOK_KEYWORD, "typeid"],
        ["typename", TOK_KEYWORD, "typename"],
        ["union", TOK_KEYWORD, "union"],
        ["unsigned", TOK_KEYWORD, "unsigned"],
        ["using", TOK_KEYWORD, "using"],
        ["virtual", TOK_KEYWORD, "virtual"],
        ["void", TOK_KEYWORD, "void"],
        ["volatile", TOK_KEYWORD, "volatile"],
        ["wchar_t", TOK_KEYWORD, "wchar_t"],
        ["while", TOK_KEYWORD, "while"],
        ["xor", TOK_PUNCTUATOR, "^"],
        ["xor_eq", TOK_PUNCTUATOR, "^="],
    ]
    for alt in alt_list:
        if ident == alt[0]:
            return (alt[1], alt[2])
    return (TOK_IDENT, ident)


def translate_punctuator(op_punct: str):
    if op_punct == '#' or op_punct == '##' or op_punct == '%:' or op_punct == '%:%:':
        print("Can't use operator '{}' in code".format(op_punct))
    if op_punct == '<:':
        op_punct = '['
    if op_punct == ':>':
        op_punct = ']'
    if op_punct == '<%':
        op_punct = '{'
    if op_punct == '%>':
        op_punct = '}'
    return (TOK_PUNCTUATOR, op_punct)
