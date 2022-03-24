use std::fmt::Display;
use std::io::stdout;

use crossterm::style::Color;
use crossterm::style::ResetColor;
use crossterm::style::SetForegroundColor;
use crossterm::ExecutableCommand;

use crate::analysis::text::span::TextSpan;
use crate::object::Object;

use super::kind::SyntaxKind;

pub mod expression;
pub mod unit;

pub trait SyntaxNode: Display {
    fn kind(&self) -> SyntaxKind;
    fn children(&self) -> Vec<&dyn SyntaxNode>;
    fn is_token(&self) -> bool {
        false
    }
    fn token_text(&self) -> &str {
        panic!("token_text() on non-token");
    }
    fn token_value(&self) -> Option<&Object> {
        panic!("token_value() on non-token");
    }
    fn pretty_print(&self) -> std::io::Result<()>
    where
        Self: Sized,
    {
        pretty_print_node(self, &mut stdout(), true, String::new(), true)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()>
    where
        Self: Sized,
    {
        pretty_print_node(self, writer, false, String::new(), true)
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
    is_to_console: bool,
    indent: String,
    is_last: bool,
) -> Result<(), std::io::Error> {
    if is_to_console {
        writer.execute(SetForegroundColor(Color::DarkGrey))?;
    }
    write!(writer, "{}", indent)?;
    let marker = if is_last {
        "└───"
    } else {
        "├───"
    };
    write!(writer, "{}", marker)?;
    if is_to_console {
        writer.execute(SetForegroundColor(if node.is_token() {
            Color::Blue
        } else {
            Color::Cyan
        }))?;
    }
    write!(writer, "{:?}", node.kind())?;
    if is_to_console {
        writer.execute(ResetColor)?;
    }
    if node.is_token() {
        if let Some(value) = node.token_value() {
            write!(writer, " {}", value)?;
        }
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
            is_to_console,
            indent.clone(),
            std::ptr::eq(child, last_child.unwrap()),
        )?;
    }
    Ok(())
}
