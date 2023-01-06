module minsk.code_analysis.syntax.syntax_token;

import minsk.runtime.object : Obj;
import minsk.code_analysis.syntax.syntax_kind : SyntaxKind;

struct SyntaxToken {
    SyntaxKind kind;
    int position;
    string text;
    Obj value;

    this(SyntaxKind kind, int position, string text, Obj value) {
        this.kind = kind;
        this.position = position;
        this.text = text;
        this.value = value;
    }
}
