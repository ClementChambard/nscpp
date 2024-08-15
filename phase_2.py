# Unify lines
def phase_2(source: str) -> str:   # should be reversible in raw strings
    out_source = ""
    while len(source) > 0:
        if source[0] != '\\':
            out_source += source[0]
            source = source[1:]
            continue
        source = source[1:]
        if len(source) > 0 and source[0] == '\\':
            out_source += '\\\\'
            source = source[1:]
            continue
        source_bis = source
        while len(source_bis) > 0:
            if not source_bis[0].isspace():
                out_source += '\\'
                break
            if source_bis[0] == '\n':
                source = source_bis[1:]
                break
            source_bis = source_bis[1:]
    if not out_source.endswith('\n'):
        out_source += '\n'
    return out_source
