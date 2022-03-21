#[derive(Clone, Copy)]
pub struct TextSpan {
    start: usize,
    length: usize,
}

impl TextSpan {
    pub fn new(start: usize, length: usize) -> Self {
        Self { start, length }
    }

    pub fn from_bounds(start: usize, end: usize) -> Self {
        Self {
            start,
            length: end - start,
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn length(&self) -> usize {
        self.length
    }

    pub fn end(&self) -> usize {
        self.start + self.length
    }
}
