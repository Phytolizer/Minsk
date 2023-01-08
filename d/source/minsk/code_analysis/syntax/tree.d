module minsk.code_analysis.syntax.tree;

import minsk.code_analysis.syntax.parser : Parser;
import minsk.code_analysis.syntax.node : ExpressionSyntax;
import minsk.code_analysis.syntax.token : SyntaxToken;

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
