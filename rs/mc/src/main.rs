use std::io::Write;
use std::io::stdin;
use std::io::stdout;
use std::io::BufRead;
use std::io::BufReader;

use crossterm::style::Color;
use crossterm::style::ResetColor;
use crossterm::style::SetForegroundColor;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use crossterm::ExecutableCommand;

use minsk::analysis::compilation::Compilation;
use minsk::analysis::syntax::node::SyntaxNode;
use minsk::analysis::syntax::tree::SyntaxTree;

fn main() -> std::io::Result<()> {
    let mut reader = BufReader::new(stdin());
    let mut line = String::new();
    let mut show_tree = false;
    loop {
        print!("> ");
        stdout().flush()?;
        line.clear();
        if reader.read_line(&mut line)? == 0 {
            break;
        }

        match line.trim() {
            "#showTree" => {
                show_tree = !show_tree;
                println!(
                    "{}",
                    if show_tree {
                        "Showing parse trees."
                    } else {
                        "Not showing parse trees."
                    }
                );
                continue;
            }
            "#cls" => {
                stdout().execute(Clear(ClearType::All))?;
                continue;
            }
            _ => {}
        }

        let syntax_tree = SyntaxTree::parse(&line);
        if show_tree && syntax_tree.diagnostics.is_empty() {
            stdout().execute(SetForegroundColor(Color::DarkGrey))?;
            syntax_tree.root.pretty_print(&mut stdout())?;
            stdout().execute(ResetColor)?;
        }
        let compilation = Compilation::new(syntax_tree);
        match compilation.evaluate() {
            Ok(result) => {
                println!("{}", result);
            }
            Err(diagnostics) => {
                stdout().execute(SetForegroundColor(Color::DarkRed))?;
                for diagnostic in diagnostics {
                    println!("{}", diagnostic);
                }
                stdout().execute(ResetColor)?;
            }
        }
    }

    Ok(())
}
