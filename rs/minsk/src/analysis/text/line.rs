use super::span::TextSpan;

pub struct TextLine {
    pub start: usize,
    pub length: usize,
    pub length_including_line_break: usize,
}

impl TextLine {
    pub fn end(&self) -> usize {
        self.start + self.length
    }

    pub fn end_including_line_break(&self) -> usize {
        self.start + self.length_including_line_break
    }

    pub fn span(&self) -> TextSpan {
        TextSpan::new(self.start, self.length)
    }

    pub fn span_including_line_break(&self) -> TextSpan {
        TextSpan::new(self.start, self.length_including_line_break)
    }
}
