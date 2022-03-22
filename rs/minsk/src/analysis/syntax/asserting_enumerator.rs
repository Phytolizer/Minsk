use super::kind::SyntaxKind;
use super::node::SyntaxNode;

pub(crate) struct AssertingEnumerator<'t> {
    enumerator: Enumerator<'t>,
}

impl<'t> AssertingEnumerator<'t> {
    pub(crate) fn new(node: &'t dyn SyntaxNode) -> Self {
        Self {
            enumerator: Enumerator::new(node),
        }
    }

    pub(crate) fn assert_node(&mut self, kind: SyntaxKind) {
        let current = self.enumerator.next().unwrap();
        assert_eq!(kind, current.kind());
        assert!(!current.is_token());
    }

    pub(crate) fn assert_token(&mut self, kind: SyntaxKind, text: &str) {
        let current = self.enumerator.next().unwrap();
        assert_eq!(kind, current.kind());
        assert!(current.is_token());
        assert_eq!(text, current.token_text());
    }

    pub(crate) fn assert_at_end(&mut self) {
        assert!(self.enumerator.next().is_none());
    }
}

struct Enumerator<'t> {
    stack: Vec<&'t dyn SyntaxNode>,
}

impl<'t> Enumerator<'t> {
    fn new(node: &'t dyn SyntaxNode) -> Self {
        Self { stack: vec![node] }
    }
}

impl<'t> Iterator for Enumerator<'t> {
    type Item = &'t dyn SyntaxNode;

    fn next(&mut self) -> Option<Self::Item> {
        let n = match self.stack.pop() {
            Some(n) => n,
            None => return None,
        };
        for child in n.children().into_iter().rev() {
            self.stack.push(child);
        }
        Some(n)
    }
}
