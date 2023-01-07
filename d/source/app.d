import std.range : empty;
import std.stdio : readln, write, writeln;
import std.string : strip;

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

		auto parser = new Parser(line);
		const expression = parser.parse();

		color(Fg.white, Style.faint);
		SyntaxNode.prettyPrint(expression);
		color(Style.reset);

		if (!parser.diagnostics.empty) {
			color(Fg.red, Style.faint);
			foreach (diagnostic; parser.diagnostics) {
				writeln(diagnostic);
			}
			color(Style.reset);
		}
	}
}
