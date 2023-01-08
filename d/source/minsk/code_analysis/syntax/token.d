module minsk.code_analysis.syntax.token;

import minsk.runtime.object : Obj;
import minsk.code_analysis.syntax.kind : SyntaxKind;
import minsk.code_analysis.syntax.node : SyntaxNode;

class SyntaxToken : SyntaxNode {
    private SyntaxKind _kind;
    int position;
    string text;
    Obj value;

    this(SyntaxKind kind, int position, string text, Obj value) {
        _kind = kind;
        this.position = position;
        this.text = text;
        this.value = value;
    }

    override SyntaxKind kind() const {
        return _kind;
    }

    override const(SyntaxNode)[] children() const {
        return [];
    }
}
