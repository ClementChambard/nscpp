# Translate chars
def phase_1(source: str) -> str:
    out_source = ""
    while len(source) > 0:
        if source[0] == '\r':
            if len(source) > 1 and source[1] == '\n':
                out_source += '\n'
                source = source[2:]
            else:
                out_source += '\n'
                source = source[1:]
        else:
            out_source += source[0]
            source = source[1:]
    return out_source
