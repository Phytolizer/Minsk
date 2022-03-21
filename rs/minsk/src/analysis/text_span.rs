#[derive(Clone, Copy)]
pub struct TextSpan {
    start: usize,
    length: usize,
}

impl TextSpan {
    pub(crate) fn new(start: usize, length: usize) -> Self {
        Self { start, length }
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
