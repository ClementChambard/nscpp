#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Location {
    pub filename: String,
    pub lines: std::ops::Range<usize>,
    pub cols: std::ops::Range<usize>,
}

impl Location {
    pub fn new(filename: &str, line: usize, col: usize) -> Self {
        Self {
            filename: String::from(filename),
            lines: line..line,
            cols: col..col,
        }
    }

    pub fn lrange(filename: &str, line: usize, cols: std::ops::Range<usize>) -> Self {
        Self {
            filename: String::from(filename),
            lines: line..line,
            cols,
        }
    }

    pub fn range(
        filename: &str,
        lines: std::ops::Range<usize>,
        cols: std::ops::Range<usize>,
    ) -> Self {
        Self {
            filename: String::from(filename),
            lines,
            cols,
        }
    }

    pub fn from_merged(l: &Self, r: &Self) -> Self {
        let min_line = l.lines.start.min(r.lines.start);
        let max_line = l.lines.end.max(r.lines.end);
        let min_col = l.cols.start.min(r.cols.start);
        let max_col = l.cols.end.max(r.cols.end);

        Self {
            filename: l.filename.clone(),
            lines: min_line..max_line,
            cols: min_col..max_col,
        }
    }

    pub fn merge_in_place(&mut self, o: &Self) {
    }

    pub fn merge(self, o: Self) -> Self {
        let min_line = self.lines.start.min(o.lines.start);
        let max_line = self.lines.end.max(o.lines.end);
        let min_col = self.cols.start.min(o.cols.start);
        let max_col = self.cols.end.max(o.cols.end);

        Self {
            filename: self.filename,
            lines: min_line..max_line,
            cols: min_col..max_col,
        }
    }

    pub fn w_file_line(self, file: &str, line_diff: usize) -> Self {
        let lines = self.lines.start + line_diff..self.lines.end + line_diff;
        Self {
            filename: String::from(file),
            lines,
            cols: self.cols,
        }
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        //if self.lines.start != self.lines.end {
        //    if self.cols.start != self.cols.end {
        //        write!(
        //            fmt,
        //            "{}:{}..{}:{}..{}",
        //            self.filename, self.lines.start, self.lines.end, self.cols.start, self.cols.end
        //        )
        //    } else {
        //        write!(
        //            fmt,
        //            "{}:{}..{}:{}",
        //            self.filename, self.lines.start, self.lines.end, self.cols.start
        //        )
        //    }
        //} else if self.cols.start != self.cols.end {
        //    write!(
        //        fmt,
        //        "{}:{}:{}..{}",
        //        self.filename, self.lines.start, self.cols.start, self.cols.end
        //    )
        //} else {
        //    write!(
        //        fmt,
        //        "{}:{}:{}",
        //        self.filename, self.lines.start, self.cols.start
        //    )
        //}
        write!(
            fmt,
            "{}:{}:{}",
            self.filename, self.lines.start, self.cols.start
        )
    }
}
