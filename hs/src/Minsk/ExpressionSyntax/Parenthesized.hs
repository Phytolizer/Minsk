module Minsk.ExpressionSyntax.Parenthesized
  ( ParenthesizedExpressionSyntax (..),
    parenthesizedExpressionSyntax,
  )
where

import BasicPrelude
import Minsk.ExpressionSyntax (ExpressionSyntax (ExpressionSyntax), IsExpressionSyntax)
import qualified Minsk.SyntaxKind as SyntaxKind
import Minsk.SyntaxNode (IsSyntaxNode (..), node)
import Minsk.SyntaxToken (SyntaxToken)

data ParenthesizedExpressionSyntax = ParenthesizedExpressionSyntax
  { openParenthesisToken :: SyntaxToken,
    expression :: ExpressionSyntax,
    closeParenthesisToken :: SyntaxToken
  }

parenthesizedExpressionSyntax :: SyntaxToken -> ExpressionSyntax -> SyntaxToken -> ExpressionSyntax
parenthesizedExpressionSyntax open e close = ExpressionSyntax $ ParenthesizedExpressionSyntax open e close

instance IsSyntaxNode ParenthesizedExpressionSyntax where
  kind _ = SyntaxKind.ParenthesizedExpression
  children (ParenthesizedExpressionSyntax open e close) =
    [node open, node e, node close]

instance IsExpressionSyntax ParenthesizedExpressionSyntax
