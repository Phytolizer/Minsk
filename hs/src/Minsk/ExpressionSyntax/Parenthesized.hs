module Minsk.ExpressionSyntax.Parenthesized
  ( ParenthesizedExpressionSyntax (..),
    parenthesizedExpressionSyntax,
  )
where

import BasicPrelude
import Minsk.ExpressionSyntax (ExpressionSyntax (ExpressionSyntax), IsExpressionSyntax)
import qualified Minsk.SyntaxKind as SyntaxKind
import Minsk.SyntaxNode (IsSyntaxNode (..), SyntaxNode (SyntaxNode), node)
import Minsk.SyntaxToken (SyntaxToken)

data ParenthesizedExpressionSyntax = ParenthesizedExpressionSyntax
  { openParenthesisToken :: SyntaxToken,
    expression :: ExpressionSyntax,
    closeParenthesisToken :: SyntaxToken
  }

parenthesizedExpressionSyntax :: SyntaxToken -> ExpressionSyntax -> SyntaxToken -> ExpressionSyntax
parenthesizedExpressionSyntax open e close = ExpressionSyntax $ ParenthesizedExpressionSyntax open e close

instance IsSyntaxNode ParenthesizedExpressionSyntax where
  node (ParenthesizedExpressionSyntax open e close) =
    SyntaxNode SyntaxKind.ParenthesizedExpression [node open, node e, node close] mempty

instance IsExpressionSyntax ParenthesizedExpressionSyntax
