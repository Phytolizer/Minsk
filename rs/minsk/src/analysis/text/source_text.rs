use std::ops::Index;
use std::ops::Range;

use super::line::TextLine;

pub struct SourceText {
    text: Vec<char>,
    pub lines: Vec<TextLine>,
}

impl SourceText {
    fn new(text: Vec<char>) -> Self {
        let mut source_text = Self {
            text,
            lines: Vec::new(),
        };

        source_text.parse_lines();

        source_text
    }

    fn parse_lines(&mut self) {
        let mut line_start = 0;
        let mut position = 0;

        while position < self.text.len() {
            let line_break_width = Self::get_line_break_width(&self.text, position);
            if line_break_width > 0 {
                self.add_line(line_start, position, line_break_width);
                position += line_break_width;
                line_start = position;
            } else {
                position += 1;
            }
        }

        self.add_line(line_start, position, 0);
    }

    fn get_line_break_width(text: &[char], position: usize) -> usize {
        let c = text[position];
        let l = text.get(position + 1).copied().unwrap_or('\0');
        match c {
            '\r' if l == '\n' => 2,
            '\r' | '\n' => 1,
            _ => 0,
        }
    }

    fn add_line(&mut self, line_start: usize, position: usize, line_break_width: usize) {
        let length = position - line_start;
        let length_including_line_break = length + line_break_width;
        self.lines.push(TextLine {
            start: line_start,
            length,
            length_including_line_break,
        });
    }

    pub fn line_index(&self, position: usize) -> usize {
        let mut lower = 0;
        let mut upper = self.lines.len() - 1;

        while lower <= upper {
            let index = lower + (upper - lower) / 2;
            let start = self.lines[index].start;

            if start == position {
                return index;
            }

            if start > position {
                upper = index - 1;
            } else {
                lower = index + 1;
            }
        }

        lower - 1
    }

    pub(crate) fn get(&self, offset: usize) -> Option<&char> {
        self.text.get(offset)
    }
}

impl From<Vec<char>> for SourceText {
    fn from(text: Vec<char>) -> Self {
        Self::new(text)
    }
}

impl ToString for SourceText {
    fn to_string(&self) -> String {
        self.text.iter().collect()
    }
}

impl Index<usize> for SourceText {
    type Output = char;

    fn index(&self, index: usize) -> &Self::Output {
        &self.text[index]
    }
}

impl Index<Range<usize>> for SourceText {
    type Output = [char];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.text[index]
    }
}
