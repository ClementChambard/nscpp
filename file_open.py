from phase_1 import phase_1
from phase_2 import phase_2
from phase_3 import phase_3


def open_file(filename: str) -> [(int, str)]:
    content = ""
    with open(filename, "r") as f:
        content = f.read()
    translated = phase_1(content)
    joined = phase_2(translated)
    pp_tokens = phase_3(joined)
    return pp_tokens
