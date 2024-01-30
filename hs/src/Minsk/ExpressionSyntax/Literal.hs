{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Minsk.ExpressionSyntax.Literal
  ( LiteralExpressionSyntax (..),
    literalExpressionSyntax,
  )
where

import BasicPrelude
import Minsk.ExpressionSyntax (ExpressionSyntax (ExpressionSyntax), IsExpressionSyntax)
import qualified Minsk.SyntaxKind as SyntaxKind
import Minsk.SyntaxNode (IsSyntaxNode (..), SyntaxNode (SyntaxNode), node)
import Minsk.SyntaxToken (SyntaxToken)

data LiteralExpressionSyntax = LiteralExpressionSyntax
  { literalToken :: SyntaxToken
  }

literalExpressionSyntax :: SyntaxToken -> ExpressionSyntax
literalExpressionSyntax t = ExpressionSyntax $ LiteralExpressionSyntax t

instance IsSyntaxNode LiteralExpressionSyntax where
  node (LiteralExpressionSyntax t) =
    SyntaxNode SyntaxKind.LiteralExpression [node t] mempty

instance IsExpressionSyntax LiteralExpressionSyntax
