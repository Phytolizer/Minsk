import std.stdio : readln, write, writeln;
import std.string : strip;

import minsk.code_analysis.syntax.lexer : Lexer;
import minsk.code_analysis.syntax.syntax_kind : SyntaxKind;

void main() {
	char[] buf;
	while (true) {
		write("> ");
		if (buf.readln() == 0)
			break;
		auto line = cast(immutable) buf.strip();

		foreach (token; new Lexer(line)) {
			write(token.kind, ": '", token.text, "'");
			if (token.value) {
				write(" ", token.value);
			}
			writeln();
		}
	}
}
