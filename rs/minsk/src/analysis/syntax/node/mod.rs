use std::fmt::Display;

use super::kind::SyntaxKind;

pub mod expression;

pub trait SyntaxNode: Display {
    fn kind(&self) -> SyntaxKind;
    fn children(&self) -> Vec<&dyn SyntaxNode>;
    fn is_token(&self) -> bool {
        false
    }
    fn pretty_print(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()>
    where
        Self: Sized,
    {
        pretty_print_node(self, writer, String::new(), true)
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
