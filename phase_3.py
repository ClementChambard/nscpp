from pp_toks import *


def preproc_other_char(text: str) -> (str, (int, str)):
    c = text[0]
    o = ord(c)
    if o >= 9 and o <= 12 or \
            o == 0x20 or o == 0x21 or o == 0x23 or o == 0x25 or o == 0x26 or \
            o >= 0x28 and o <= 0x7e:
        return (text[1:], (PPT_OTHER, c))
    err_pr(text, "Invalid character")


def parse_preproc_tok(text: str) -> (str, (int, str)):
    charlit = try_parse_preproc_charlit(text)
    if charlit is not None:
        text, charlit = charlit
        return (text, (PPT_CHAR, charlit))
    strlit = try_parse_preproc_strlit(text)
    if strlit is not None:
        text, strlit = strlit
        return (text, (PPT_STRING, strlit))
    if is_ident_start(text[0]):
        text, id = parse_preproc_ident(text)
        return (text, (PPT_IDENT, id))
    if text[0].isdigit() or \
       (len(text) > 1 and text[0] == '.' and text[1].isdigit()):
        text, num = parse_preproc_number(text)
        return (text, (PPT_NUMBER, num))
    op_punct = try_parse_preproc_op_punct(text)
    if op_punct is not None:
        text, op_punct = op_punct
        return (text, (PPT_OP_PUNCT, op_punct))
    if text[0].isspace():
        nl = text[0] == '\n'
        text = text[1:]
        while len(text) > 0 and text[0].isspace():
            if text[0] == '\n':
                nl = True
            text = text[1:]
        if nl:
            return (text, (PPT_WHITESPACE_NL, ""))
        return (text, (PPT_WHITESPACE, ""))
    return preproc_other_char(text)


def try_read_header(text: str, last_tok: (int, str)) -> (str, str):
    out = ""
    if last_tok[0] == PPT_OP_PUNCT and last_tok[1] == "<":
        while len(text) > 0:
            if text[0] == '\n' or text[0] == '>':
                break
            out += text[0]
            text = text[1:]
        return (text, out)
    elif text.startswith('"'):
        text = text[1:]
        while len(text) > 0:
            if text[0] == '\n' or text[0] == '"':
                break
            out += text[0]
            text = text[1:]
        return (text[1:], out)
    return None


# Lexing
def phase_3(source: str) -> [(int, str)]:
    can_read_header = False
    out = []
    while len(source) > 0:
        if source.startswith("//"):
            source = source[2:]
            while len(source) > 0 and source[0] != "\n":
                source = source[1:]
            source = "\n" + source[1:]
        if source.startswith("/*"):
            source = source[2:]
            while len(source) > 0 and not source.startswith("*/"):
                source = source[1:]
            source = " " + source[2:]
        if can_read_header:
            h = try_read_header(source, out[-1])
            if h is not None:
                source, h = h
                out.append((PPT_HEADERNAME, h))
                can_read_header = False
                continue
        source, (tok_ty, tok_val) = parse_preproc_tok(source)
        if tok_ty == "#":
            can_read_header = True
        elif can_read_header and tok_ty == PPT_IDENT and tok_val == "include":
            can_read_header = True
        elif can_read_header and ((tok_ty == PPT_OP_PUNCT and
                                  tok_val == "<") or tok_ty == PPT_WHITESPACE):
            can_read_header = True
        else:
            can_read_header = False
        out.append((tok_ty, tok_val))
    return out

