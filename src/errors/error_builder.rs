use crate::util::color;

struct Underline {
    range: std::ops::Range<usize>,
    c: char,
    col: color::Color,
    txt_col: Option<color::Color>,
}

struct FileUnderline {
    filename: String,
    line: usize,
    underlines: Vec<Underline>,
}

fn get_file_line(filename: &str, line: usize) -> String {
    use std::fs::File;
    use std::io::{BufRead, BufReader};
    let file = File::open(filename).expect("Unable to open file");
    let reader = BufReader::new(file);

    for (index, s) in reader.lines().enumerate() {
        if index == line {
            return s.unwrap();
        }
    }
    panic!("Error inside error reporting system!!!!");
}

impl FileUnderline {
    pub fn build(self) -> String {
        let u = self.underlines.first().unwrap();
        let line1 = get_file_line(&self.filename, self.line);
        let line2 = color::format_str()
            .s(&String::from(" ").repeat(u.range.start))
            .s_c(&String::from(u.c).repeat(u.range.end - u.range.start), u.col);
        format!("{:>4} | {}\n     | {}", self.line, line1, line2)
    }
}

struct SimpleMessage {
    message: String,
}

impl SimpleMessage {
    pub fn build(self) -> String {
        self.message
    }
}

enum ErrorMessage {
    Simple(SimpleMessage),
    FileUnderline(FileUnderline),
}

impl ErrorMessage {
    pub fn build(self) -> String {
        match self {
            Self::Simple(m) => m.build(),
            Self::FileUnderline(m) => m.build(),
        }
    }
}

pub struct ErrorBuilder {
    messages: Vec<ErrorMessage>,
}

impl ErrorBuilder {
    pub fn new() -> Self {
        ErrorBuilder { messages: vec![] }
    }

    pub fn message(mut self, s: &str) -> Self {
        self.messages.push(ErrorMessage::Simple(SimpleMessage {
            message: String::from(s),
        }));
        self
    }

    pub fn error(mut self, s: &str) -> Self {
        self.messages.push(ErrorMessage::Simple(SimpleMessage {
            message: color::format_str().s_c("error:", color::RED).c(' ').s(s).build(),
        }));
        self
    }

    pub fn error_at_loc(mut self, s: &str, loc: &crate::util::loc::Location) -> Self {
        self.messages.push(ErrorMessage::Simple(SimpleMessage {
            message: color::format_str().put(loc).c(' ').s_c("error:", color::RED).c(' ').s(s).build(),
        }));
        self.messages
            .push(ErrorMessage::FileUnderline(FileUnderline {
                filename: loc.filename.clone(),
                line: loc.lines.start,
                underlines: vec![Underline {
                    range: loc.cols.start..loc.cols.end + 1,
                    c: '^',
                    col: color::RED,
                    txt_col: None,
                }],
            }));
        self
    }

    pub fn build(self) -> String {
        let mut out = String::new();
        for m in self.messages {
            out.push_str(&m.build());
            out.push('\n');
        }
        out
    }

    pub fn panic(self) -> ! {
        print!("{}", self.build());
        panic!("an error occured: Aborting!");
    }
}
