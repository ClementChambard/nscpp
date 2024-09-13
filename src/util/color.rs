#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Color {
    BLACK,
    RED,
    GREEN,
    YELLOW,
    BLUE,
    PURPLE,
    CYAN,
    WHITE,
}

pub use Color::*;

impl Color {
    fn fg_code(&self) -> i32 {
        match &self {
            BLACK => 30,
            RED => 31,
            GREEN => 32,
            YELLOW => 33,
            BLUE => 34,
            PURPLE => 35,
            CYAN => 36,
            WHITE => 37,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum FormatContent {
    Data(String),
    Char(char),
    Fg(Color),
    _Bg(Color),
    Bold(bool),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Formatter {
    content: Vec<FormatContent>,
}

impl Formatter {
    pub fn new() -> Self {
        Self {
            content: Vec::new(),
        }
    }

    pub fn put<T: std::fmt::Display>(self, v: &T) -> Self {
        self.s(&format!("{v}"))
    }

    pub fn put_b<T: std::fmt::Display>(self, v: &T) -> Self {
        self.bold(true).put(v).reset()
    }

    pub fn put_c<T: std::fmt::Display>(self, v: &T, c: Color) -> Self {
        self.col(c).put(v).reset()
    }

    pub fn put_cb<T: std::fmt::Display>(self, v: &T, c: Color) -> Self {
        self.bold(true).col(c).put(v).reset()
    }

    pub fn put_d<T: std::fmt::Debug>(self, v: &T) -> Self {
        self.s(&format!("{v:?}"))
    }

    pub fn put_db<T: std::fmt::Debug>(self, v: &T) -> Self {
        self.bold(true).put_d(v).reset()
    }

    pub fn put_dc<T: std::fmt::Debug>(self, v: &T, c: Color) -> Self {
        self.col(c).put_d(v).reset()
    }

    pub fn put_dcb<T: std::fmt::Debug>(self, v: &T, c: Color) -> Self {
        self.bold(true).col(c).put_d(v).reset()
    }

    pub fn s(mut self, s: &str) -> Self {
        self.content.push(FormatContent::Data(String::from(s)));
        self
    }

    pub fn s_c(self, s: &str, c: Color) -> Self {
        self.col(c).s(s).reset()
    }

    pub fn s_b(self, s: &str) -> Self {
        self.bold(true).s(s).reset()
    }

    pub fn s_cb(self, s: &str, c: Color) -> Self {
        self.bold(true).col(c).s(s).reset()
    }

    pub fn c(mut self, c: char) -> Self {
        self.content.push(FormatContent::Char(c));
        self
    }

    pub fn col(mut self, c: Color) -> Self {
        self.content.push(FormatContent::Fg(c));
        self
    }

    pub fn bold(mut self, bold: bool) -> Self {
        self.content.push(FormatContent::Bold(bold));
        self
    }

    pub fn reset(self) -> Self {
        self.bold(false).col(WHITE)
    }

    pub fn build(self) -> String {
        struct State {
            last_render_color: Color,
            cur_color: Color,
            last_render_bold: bool,
            cur_bold: bool,
            out: String,
        }
        let mut state = State {
            last_render_color: WHITE,
            cur_color: WHITE,
            last_render_bold: false,
            cur_bold: false,
            out: String::new(),
        };
        fn do_draw(s: &mut State) {
            if (s.cur_color != s.last_render_color || s.cur_bold != s.last_render_bold) &&
                s.cur_color == WHITE && s.cur_bold == false {
                s.out.push_str("\x1b[0m");
                s.last_render_color = s.cur_color;
                s.last_render_bold = s.cur_bold;
                return;
            }
            if s.cur_color != s.last_render_color && s.cur_bold != s.last_render_bold{
                s.out.push_str(&format!("\x1b[{};{}m", if s.cur_bold { 1 } else { 22 }, s.cur_color.fg_code()));
                s.last_render_color = s.cur_color;
                s.last_render_bold = s.cur_bold;
                return;
            }
            if s.cur_color != s.last_render_color {
                s.out.push_str(&format!("\x1b[{}m", s.cur_color.fg_code()));
                s.last_render_color = s.cur_color;
            } else if s.cur_bold != s.last_render_bold {
                s.out.push_str(&format!("\x1b[{}m", if s.cur_bold { 1 } else { 22 }));
                s.last_render_bold = s.cur_bold;
            }
        }
        for c in self.content {
            match c {
                FormatContent::Bold(b) => state.cur_bold = b,
                FormatContent::Fg(c) => state.cur_color = c,
                FormatContent::Char(c) => {
                    do_draw(&mut state);
                    state.out.push(c);
                }
                FormatContent::Data(s) => {
                    do_draw(&mut state);
                    state.out.push_str(&s);
                }
                _ => { unimplemented!() }
            }
        }
        state.cur_color = WHITE;
        state.cur_bold = false;
        do_draw(&mut state);
        state.out
    }
}

impl std::fmt::Display for Formatter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.clone().build())
    }
}

impl From<Formatter> for String {
    fn from(value: Formatter) -> Self {
        value.build()
    }
}

pub fn format_str() -> Formatter {
    Formatter::new()
}
