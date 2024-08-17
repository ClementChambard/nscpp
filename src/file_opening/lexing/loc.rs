#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Location<'a> {
    filename: &'a str,
    lines: std::ops::Range<usize>,
    cols: std::ops::Range<usize>,
}

impl<'a> Location<'a> {
    pub fn new(filename: &'a str, line: usize, col: usize) -> Self {
        Self {
            filename,
            lines: line..line,
            cols: col..col,
        }
    }

    pub fn lrange(filename: &'a str, line: usize, cols: std::ops::Range<usize>) -> Self {
        Self {
            filename,
            lines: line..line,
            cols,
        }
    }

    pub fn range(
        filename: &'a str,
        lines: std::ops::Range<usize>,
        cols: std::ops::Range<usize>,
    ) -> Self {
        Self {
            filename,
            lines,
            cols,
        }
    }
}

impl<'a> std::fmt::Display for Location<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        if self.lines.start != self.lines.end {
            if self.cols.start != self.cols.end {
                write!(
                    fmt,
                    "{}:{}..{}:{}..{}",
                    self.filename, self.lines.start, self.lines.end, self.cols.start, self.cols.end
                )
            } else {
                write!(
                    fmt,
                    "{}:{}..{}:{}",
                    self.filename, self.lines.start, self.lines.end, self.cols.start
                )
            }
        } else if self.cols.start != self.cols.end {
            write!(
                fmt,
                "{}:{}:{}..{}",
                self.filename, self.lines.start, self.cols.start, self.cols.end
            )
        } else {
            write!(
                fmt,
                "{}:{}:{}",
                self.filename, self.lines.start, self.cols.start
            )
        }
    }
}
