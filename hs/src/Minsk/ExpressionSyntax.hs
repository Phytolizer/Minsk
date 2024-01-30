module Minsk.ExpressionSyntax
  ( IsExpressionSyntax,
    ExpressionSyntax (..),
  )
where

import Minsk.SyntaxNode (IsSyntaxNode (..))

class (IsSyntaxNode n) => IsExpressionSyntax n

data ExpressionSyntax = forall n. (IsExpressionSyntax n) => ExpressionSyntax n

instance IsSyntaxNode ExpressionSyntax where
  node (ExpressionSyntax n) = node n
