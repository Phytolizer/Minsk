import std.range : empty;
import std.stdio : readln, write, writeln;
import std.string : strip;

import minsk.code_analysis.binding : Binder;
import minsk.code_analysis.evaluator : Evaluator;
import minsk.code_analysis.syntax : Lexer,
	SyntaxKind,
	SyntaxNode,
	SyntaxTree,
	prettyPrint;
import minsk.support.color : color, Fg, Style;

void main() {
	char[] buf;
	auto showTree = false;

	while (true) {
		write("> ");
		if (buf.readln() == 0) {
			writeln();
			break;
		}
		auto line = cast(immutable) buf.strip();

		if (line == "#showTree") {
			showTree = !showTree;
			writeln(showTree ? "Showing parse trees." : "Not showing parse trees.");
			continue;
		}
		if (line == "#cls") {
			write("\x1b[2J\x1b[H");
			continue;
		}

		const syntaxTree = SyntaxTree.parse(line);
		auto binder = new Binder();
		const boundExpression = binder.bindExpression(syntaxTree.root);
		const diagnostics = syntaxTree.diagnostics ~ binder.diagnostics;

		if (showTree) {
			color(Fg.white, Style.faint);
			prettyPrint(syntaxTree.root);
			color(Style.reset);
		}

		if (diagnostics.empty) {
			color(Fg.magenta);
			writeln(new Evaluator(boundExpression).evaluate());
			color(Style.reset);
		} else {
			color(Fg.red, Style.faint);
			foreach (diagnostic; diagnostics) {
				writeln(diagnostic);
			}
			color(Style.reset);
		}
	}
}
