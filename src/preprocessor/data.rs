#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PPMacro {
    pub name: String,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PPData {
    pub macros: Vec<PPMacro>,
    pub cond_stack: Vec<bool>,
    pub cur_file: String,
    pub line_diff: usize,
}

fn default_macros() -> Vec<PPMacro> {
    vec![
        // TODO:
    ]
}

impl PPData {
    pub fn new(filename: &str) -> Self {
        Self {
            macros: default_macros(),
            cond_stack: vec![true],
            cur_file: String::from(filename),
            line_diff: 0,
        }
    }

    pub fn with_macros(filename: &str, macros: &[PPMacro]) -> Self {
        Self {
            macros: Vec::from(macros),
            cond_stack: vec![true],
            cur_file: String::from(filename),
            line_diff: 0,
        }
    }
}
