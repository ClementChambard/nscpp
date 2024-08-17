use super::parse_one;

pub fn open_file(filename: &str) -> Vec<super::PPToken> {
    let source = std::fs::read_to_string(filename).unwrap();
    let source = super::translation(source);
    let lines = super::line_join(source);
    println!("{:?}", lines);
    let mut lines_ref = &lines[..];
    let mut col = 0;
    let mut tokens = Vec::new();
    while !lines_ref.is_empty() {
        let (out_lines, out_col, out_tok) = parse_one(filename, lines_ref, col);
        lines_ref = out_lines;
        col = out_col;
        tokens.push(out_tok);
    }
    tokens
}
