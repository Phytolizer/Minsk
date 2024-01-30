module Minsk.ExpressionSyntax.Binary
  ( BinaryExpressionSyntax (..),
    binaryExpressionSyntax,
  )
where

import BasicPrelude
import Minsk.ExpressionSyntax (ExpressionSyntax (ExpressionSyntax), IsExpressionSyntax)
import qualified Minsk.SyntaxKind as SyntaxKind
import Minsk.SyntaxNode (IsSyntaxNode (..), SyntaxNode (SyntaxNode), node)
import Minsk.SyntaxToken (SyntaxToken)

data BinaryExpressionSyntax = BinaryExpressionSyntax
  { left :: ExpressionSyntax,
    operatorToken :: SyntaxToken,
    right :: ExpressionSyntax
  }

binaryExpressionSyntax :: ExpressionSyntax -> SyntaxToken -> ExpressionSyntax -> ExpressionSyntax
binaryExpressionSyntax l op r = ExpressionSyntax $ BinaryExpressionSyntax l op r

instance IsSyntaxNode BinaryExpressionSyntax where
  node (BinaryExpressionSyntax l op r) =
    SyntaxNode SyntaxKind.BinaryExpression [node l, node op, node r] mempty

instance IsExpressionSyntax BinaryExpressionSyntax
