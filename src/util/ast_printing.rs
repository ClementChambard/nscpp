pub use super::color;
use super::loc::Location;

pub fn start_str_with_loc(color: color::Color, name: &str, fstr: &str, loc: &Location) -> String {
    color::format_str().s_c(fstr, color::BLUE).s_cb(name, color)
        .reset().s(" <")
        .col(color::YELLOW).s("line:").put(&loc.lines.start).c(':').put(&loc.cols.start).reset()
        .s(", ")
        .col(color::YELLOW).s("line:").put(&loc.lines.end).c(':').put(&loc.cols.end).reset()
        .c('>').build()
}

pub fn start_str(color: color::Color, name: &str, fstr: &str) -> String {
    color::format_str().s_c(fstr, color::BLUE).s_cb(name, color).build()
}

pub trait AstPrint {
    fn print_ast(&self, fstr: &str, lstr: &str) -> String;
    fn print_ast_mid(&self, lstr: &str) -> String {
        self.print_ast(&format!("{lstr}├─ "), &format!("{lstr}│  "))
    }
    fn print_ast_end(&self, lstr: &str) -> String {
        self.print_ast(&format!("{lstr}╰─ "), &format!("{lstr}   "))
    }
}

pub fn print_ast_list_opt<T: AstPrint>(nodes: &[T], lstr: &str) -> Option<String> {
    let out = print_ast_list(nodes, lstr);
    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

pub fn print_ast_list<T: AstPrint>(nodes: &[T], lstr: &str) -> String {
    let mut out = String::new();
    if nodes.len() == 0 {
        return out;
    }
    for n in nodes.iter().take(nodes.len() - 1) {
        out.push_str(&n.print_ast_mid(lstr));
        out.push('\n');
    }
    out.push_str(&nodes.last().unwrap().print_ast_end(lstr));
    out
}
