#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SourceLine {
    Line(usize, String),
    Join(String),
}

pub fn line_join(source: String) -> Vec<SourceLine> {
    let mut out_source = String::new();
    let mut lines = Vec::new();
    let mut chars = source.chars();
    let mut line = 0;
    while let Some(c) = chars.next() {
        if c != '\\' && c != '\n' {
            out_source.push(c);
            continue;
        }

        if c == '\n' {
            lines.push(SourceLine::Line(line, out_source));
            line += 1;
            out_source = String::new();
            continue;
        }

        let mut temp_str = String::from(c);
        // Warning is not true
        //#[allow(lint)]
        for c in &mut chars {
            if !c.is_whitespace() {
                out_source.push_str(&temp_str);
                out_source.push(c);
                break;
            }
            temp_str.push(c);
            if c == '\n' {
                lines.push(SourceLine::Line(line, out_source));
                out_source = String::new();
                lines.push(SourceLine::Join(temp_str));
                line += 1;
                break;
            }
        }
    }
    if !out_source.is_empty() {
        lines.push(SourceLine::Line(line, out_source));
    }
    lines
}
