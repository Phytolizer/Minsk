import std.range : empty;
import std.stdio : readln, write, writeln;
import std.string : strip;

import minsk.code_analysis.evaluator : Evaluator;
import minsk.code_analysis.syntax : Lexer, Parser, SyntaxKind, SyntaxNode;
import minsk.support.color : color, Fg, Style;

void main() {
	char[] buf;
	while (true) {
		write("> ");
		if (buf.readln() == 0) {
			writeln();
			break;
		}
		auto line = cast(immutable) buf.strip();

		const syntaxTree = new Parser(line).parse();

		color(Fg.white, Style.faint);
		SyntaxNode.prettyPrint(syntaxTree.root);
		color(Style.reset);

		if (!syntaxTree.diagnostics.empty) {
			color(Fg.red, Style.faint);
			foreach (diagnostic; syntaxTree.diagnostics) {
				writeln(diagnostic);
			}
			color(Style.reset);
		} else {
			color(Fg.magenta);
			writeln(new Evaluator(syntaxTree.root).evaluate());
			color(Style.reset);
		}
	}
}
