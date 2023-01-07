module minsk.code_analysis.syntax.syntax_tree;

import minsk.code_analysis.syntax.parser : Parser;
import minsk.code_analysis.syntax.syntax_node : ExpressionSyntax;
import minsk.code_analysis.syntax.syntax_token : SyntaxToken;

struct SyntaxTree {
    const(ExpressionSyntax) root;
    const(SyntaxToken) endOfFileToken;
    const(string[]) diagnostics;

    this(
        const(ExpressionSyntax) root,
        const(SyntaxToken) endOfFileToken,
        const(string[]) diagnostics,
    ) {
        this.root = root;
        this.endOfFileToken = endOfFileToken;
        this.diagnostics = diagnostics;
    }

    static SyntaxTree parse(string text) {
        return new Parser(text).parse();
    }
}
