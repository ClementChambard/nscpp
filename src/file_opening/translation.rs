// Translate chars

// Should check if valid utf-8
// Replace \r and \r\n by \n

pub fn translation(source: String) -> String {
    let mut out_source = String::new();
    let mut chars = source.chars();
    while let Some(c) = chars.next() {
        if c == '\r' {
            match chars.next() {
                Some('\n') => out_source.push('\n'),
                None => out_source.push('\n'),
                Some(c) => {
                    out_source.push('\n');
                    out_source.push(c);
                }
            }
        } else {
            out_source.push(c);
        }
    }
    out_source
}
