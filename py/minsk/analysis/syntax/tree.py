from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.token import SyntaxToken


class SyntaxTree:
    root: ExpressionSyntax
    end_of_file_token: SyntaxToken
    diagnostics: tuple[str, ...]

    def __init__(
        self,
        root: ExpressionSyntax,
        end_of_file_token: SyntaxToken,
        diagnostics: tuple[str, ...],
    ):
        self.root = root
        self.end_of_file_token = end_of_file_token
        self.diagnostics = diagnostics
