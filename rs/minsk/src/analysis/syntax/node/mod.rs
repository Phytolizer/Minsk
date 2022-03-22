use std::fmt::Display;

use crate::analysis::text_span::TextSpan;

use super::kind::SyntaxKind;

pub mod expression;

pub trait SyntaxNode: Display {
    fn kind(&self) -> SyntaxKind;
    fn children(&self) -> Vec<&dyn SyntaxNode>;
    fn is_token(&self) -> bool {
        false
    }
    fn token_text(&self) -> &str {
        panic!("token_text() on non-token");
    }
    fn pretty_print(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()>
    where
        Self: Sized,
    {
        pretty_print_node(self, writer, String::new(), true)
    }
    fn span(&self) -> TextSpan {
        let children = self.children();
        let first_span = children.first().unwrap().span();
        let last_span = children.last().unwrap().span();

        TextSpan::from_bounds(first_span.start(), last_span.end())
    }
}

fn pretty_print_node(
    node: &dyn SyntaxNode,
    writer: &mut dyn std::io::Write,
    indent: String,
    is_last: bool,
) -> Result<(), std::io::Error> {
    write!(writer, "{}", indent)?;
    let marker = if is_last {
        "└───"
    } else {
        "├───"
    };
    write!(writer, "{}", marker)?;
    if node.is_token() {
        write!(writer, "{}", node)?;
    } else {
        write!(writer, "{:?}", node.kind())?;
    }
    writeln!(writer)?;
    let indent = if is_last {
        indent + "    "
    } else {
        indent + "│   "
    };
    let children = node.children();
    let last_child = children.last().copied();
    for child in children.into_iter() {
        pretty_print_node(
            child,
            writer,
            indent.clone(),
            std::ptr::eq(child, last_child.unwrap()),
        )?;
    }
    Ok(())
}
