use std::collections::HashMap;
use std::io::stdin;
use std::io::stdout;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;

use crossterm::style::Color;
use crossterm::style::ResetColor;
use crossterm::style::SetForegroundColor;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use crossterm::ExecutableCommand;

use minsk::analysis::compilation::Compilation;
use minsk::analysis::syntax::node::SyntaxNode;
use minsk::analysis::syntax::tree::SyntaxTree;
use minsk::analysis::text::span::TextSpan;
use minsk::analysis::variable_symbol::VariableSymbol;
use minsk::object::Object;

fn main() -> std::io::Result<()> {
    let mut reader = BufReader::new(stdin());
    let mut line = String::new();
    let mut show_tree = false;
    let mut variables = HashMap::<VariableSymbol, Object>::new();
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
            syntax_tree.root.pretty_print()?;
            stdout().execute(ResetColor)?;
        }
        let compilation = Compilation::new(&syntax_tree);
        match compilation.evaluate(&mut variables) {
            Ok(result) => {
                println!("{}", result);
            }
            Err(diagnostics) => {
                for diagnostic in diagnostics {
                    stdout().execute(SetForegroundColor(Color::DarkRed))?;
                    let line_index = syntax_tree
                        .source_text
                        .line_index(diagnostic.span().start());
                    let line_number = line_index + 1;
                    let character = diagnostic.span().start()
                        - syntax_tree.source_text.lines[line_index].start
                        + 1;
                    print!("({}, {}): ", line_number, character);
                    println!("{}", diagnostic);
                    println!();
                    let error_span = diagnostic.span();
                    let prefix_span = TextSpan::new(0, error_span.start());
                    let suffix_span = TextSpan::from_bounds(error_span.end(), line.trim().len());
                    stdout().execute(ResetColor)?;
                    print!("    {}", &line[prefix_span.start()..prefix_span.end()]);
                    stdout().execute(SetForegroundColor(Color::DarkRed))?;
                    print!("{}", &line[error_span.start()..error_span.end()]);
                    stdout().execute(ResetColor)?;
                    println!("{}", &line[suffix_span.start()..suffix_span.end()]);
                    println!();
                }
                stdout().execute(ResetColor)?;
            }
        }
    }

    Ok(())
}
