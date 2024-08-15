from parse import parse
from toks import tok_parse_number, tok_parse_char, tok_parse_string
from toks import translate_punctuator, translate_ident
from pp_toks import PPT_NUMBER, PPT_CHAR, PPT_STRING, PPT_IDENT, PPT_OP_PUNCT

# TODO: Phase 2: keep track of joined lines
#       Phase 3: un-join lines inside R"()" strings
#       Phase 3: keep track of file, line and column for tokens
#       Phase 4: #file and #line directives: change apparent file and line for
#                tokens
#       Phase 4: __has_include and __has_cpp_attrib
#       Phase 4: lookup real include path
#       Phase 4: '##' operator in macros
#       Phase 7: char translation
#
# TODO: Better error reporting

def translate_tokens(pp_tokens):
    tokens = []
    for ty, v in pp_tokens:
        if ty == PPT_NUMBER:
            tokens.append(tok_parse_number(v))
        elif ty == PPT_CHAR:
            tokens.append(tok_parse_char(v))
        elif ty == PPT_STRING:
            tokens.append(tok_parse_string(v))
        elif ty == PPT_IDENT:
            tokens.append(translate_ident(v))
        elif ty == PPT_OP_PUNCT:
            tokens.append(translate_punctuator(v))
    return tokens


def phase_7(pp_tokens):
    return parse(translate_tokens(pp_tokens))
