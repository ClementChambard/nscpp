# Preprocessing tokens:
PPT_IDENT = 0
PPT_NUMBER = 1
PPT_CHAR = 2
PPT_STRING = 3
PPT_OP_PUNCT = 4
PPT_OTHER = 5
PPT_WHITESPACE = 6
PPT_WHITESPACE_NL = 7
PPT_HEADERNAME = 8


# TODO:
def is_valid_pp_tok(text: str) -> bool:
    return False


def err_pr(text: str, msg: str):
    print("{} ({}...)".format(msg, text[:5]))


def is_ident_start(c: str) -> bool:
    return c.isalpha() or c == '_'  # or unicode that has XID_start


def is_ident_continue(c: str) -> bool:
    return c.isalnum() or c == '_'  # or unicode that has XID_continue


def parse_preproc_ident(text: str) -> (str, str):
    # or unicode that has XID_start
    if not is_ident_start(text[0]):
        return (text, "")
    ident = text[0]
    text = text[1:]
    while len(text) > 0 and is_ident_continue(text[0]):
        ident += text[0]
        text = text[1:]
    return (text, ident)


def parse_preproc_number(text: str) -> (str, str):
    num = ""
    if text[0] == '.':
        num += '.'
        text = text[1:]
    num += text[0]
    text = text[1:]
    while len(text) > 0:
        if len(text) > 1 and text[0] in 'PpEe' and text[1] in '+-':
            num += text[0:2]
            text = text[2:]
            continue
        if is_ident_continue(text[0]) or text[0] == '.':
            num += text[0]
            text = text[1:]
            continue
        if len(text) > 1 and text[0] == '\'' and \
           (text[1].isalnum() or text[1] == '_'):
            num += text[0:2]
            text = text[2:]
            continue
        break
    return (text, num)


def try_read_escape_sequence(text: str) -> (str, str):
    if text[0] != '\\' or len(text) == 1:
        return None
    escape = '\\'
    text = text[1:]
    if text[0] in "'\"\\?abfnrtv":
        escape += text[0]
        text = text[1:]
        return (text, escape)
    if text[0] in "01234567":
        escape += text[0]
        text = text[1:]
        if text[0] in "01234567":
            escape += text[0]
            text = text[1:]
        if text[0] in "01234567":
            escape += text[0]
            text = text[1:]
        return (text, escape)
    if text.startswith("o{"):
        escape += text[0:2]
        text = text[2:]
        while len(text) > 0 and text[0] in "01234567":
            escape += text[0]
            text = text[1:]
        if len(text) == 0 or text[0] != "}":
            print("Error reading escape sequence")
            exit(1)
        escape += text[0]
        text = text[1:]
        return (text, escape)
    if text.startswith("x"):
        escape += text[0]
        text = text[1:]
        par = False
        if len(text) > 0 and text[0] == "{":
            escape += text[0]
            text = text[1:]
            par = True
        while len(text) > 0 and text[0] in "0123456789abcdefABCDEF":
            escape += text[0]
            text = text[1:]
        if par and (len(text) == 0 or text[0] != "}"):
            print("Error reading escape sequence")
            exit(1)
        if par:
            escape += text[0]
            text = text[1:]
        return (text, escape)
    if text.startswith("u{"):
        escape += text[0:2]
        text = text[2:]
        while len(text) > 0 and text[0] in "0123456789abcdefABCDEF":
            escape += text[0]
            text = text[1:]
        if len(text) == 0 or text[0] != "}":
            print("Error reading escape sequence")
            exit(1)
        escape += text[0]
        text = text[1:]
        return (text, escape)
    if text.startswith("u") and len(text) > 4:
        escape += text[0:5]
        text = text[5:]
        return (text, escape)
    if text.startswith("U") and len(text) > 8:
        escape += text[0:9]
        text = text[9:]
        return (text, escape)
    if text.startswith("N{"):
        escape += text[0:2]
        text = text[2:]
        while len(text) > 0 and text[0] != "}":
            escape += text[0]
            text = text[1:]
        if len(text) == 0:
            print("Error reading escape sequence")
            exit(1)
        escape += text[0]
        text = text[1:]
        return (text, escape)
    return None


def try_parse_preproc_charlit(text: str) -> (str, str):
    def read_c_char(text: str) -> (str, str):
        if text[0] != "'" and text[0] != "\\" and text[0] != "\n":
            return (text[1:], text[0])
        return try_read_escape_sequence(text)
    char = ""
    can_multi = False
    if text.startswith("'"):
        char += "'"
        text = text[1:]
        can_multi = True
    elif text.startswith("u8'"):
        char += text[0:3]
        text = text[3:]
    elif text.startswith("u'") or text.startswith("U'") or \
            text.startswith("L'"):
        char += text[0:2]
        text = text[2:]
    else:
        return None

    c = read_c_char(text)
    if c is None:
        err_pr(text, "invalid c-char")
        exit(1)
    text, c = c
    char += c

    if can_multi:
        while not text.startswith("'"):
            c = read_c_char(text)
            if c is None:
                err_pr(text, "invalid c-char")
                exit(1)
            text, c = c
            char += c

    if not text.startswith("'"):
        err_pr(text, "missing closing \"'\" character")
        exit(1)
    char += "'"
    text = text[1:]
    return (text, char)


def try_parse_preproc_strlit(text: str) -> (str, str):
    def read_s_char(text: str) -> (str, str):
        if text[0] != '"' and text[0] != '\\' and text[0] != '\n':
            return (text[1:], text[0])
        return try_read_escape_sequence(text)

    def read_d_char(text: str) -> (str, str):
        if not text[0].isspace() and text[0] not in "\\()":
            return (text[1:], text[0])
        return None

    def read_char_seq(text: str, f) -> (str, str):
        seq = ""
        while len(text) > 0:
            c = f(text)
            if c is None:
                break
            text, c = c
            seq += c
        return (text, seq)

    is_raw = False
    string = ""
    if text.startswith('"'):
        string += text[0]
        text = text[1:]
    elif text.startswith('R"'):
        string += text[0:2]
        text = text[2:]
        is_raw = True
    elif text.startswith('L"') or text.startswith('u"') or text.startswith('U"'):
        string += text[0:2]
        text = text[2:]
    elif text.startswith('LR"') or text.startswith('uR"') or text.startswith('UR"'):
        string += text[0:3]
        text = text[3:]
        is_raw = True
    elif text.startswith('u8"'):
        string += text[0:3]
        text = text[3:]
    elif text.startswith('u8R"'):
        string += text[0:4]
        text = text[4:]
        is_raw = True
    else:
        return None

    if is_raw:
        text, d_char_seq = read_char_seq(text, read_d_char)
        string += d_char_seq
        if not text.startswith('('):
            err_pr(text, "missing '(' character in raw string")
            exit(1)
        string += '('
        text = text[1:]
        closing_sequence = f"){d_char_seq}\""
        while len(text) > 0:
            if text.startswith(closing_sequence):
                string += ')' + d_char_seq
                text = text[1+len(d_char_seq):]
                break
            string += text[0]
            text = text[1:]

        if not text.startswith('"'):
            err_pr(text, "missing closing sequence '{}' in raw string".format(closing_sequence))
            exit(1)
    else:
        text, s = read_char_seq(text, read_s_char)
        string += s

        if not text.startswith('"'):
            err_pr(text, "missing closing '\"' character in string")
            exit(1)

    string += '"'
    text = text[1:]
    return (text, string)


PP_OP_PUNCT_LIST = [
    '...',
    '::',
    '.*',
    '->*',
    '->',
    '+=',
    '-=',
    '*=',
    '/=',
    '%=',
    '^=',
    '&=',
    '|=',
    '==',
    '!=',
    '<=>',
    '<=',
    '>=',
    '>>=',
    '<<=',
    '>>',
    '<<',
    '&&',
    '||',
    '++',
    '--',
    '##',
    '%:%:', # eq '##'
    '#',
    '%:', # eq '#'
    '{',
    '<%', # eq '{'
    '}',
    '%>', # eq '}'
    '[', 
    '<:', # eq '['
    ']', 
    ':>', # eq ']'
    '(',
    ')',
    ';',
    ':',
    '?',
    '.',
    '~',
    '!',
    '+',
    '-',
    '*',
    '/',
    '%',
    '^',
    '&',
    '|',
    '=',
    '<',
    '>',
    ',',
]


def try_parse_preproc_op_punct(text: str) -> (str, str):
    if text.startswith("<::"):
        if not text.startswith("<:::") and not text.startswith("<::>"):
            return (text[1:], "<")
    for sl in PP_OP_PUNCT_LIST:
        if text.startswith(sl):
            return (text[len(sl):], sl)
    return None
