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
import Minsk.SyntaxNode (IsSyntaxNode (..), node)
import Minsk.SyntaxToken (SyntaxToken)

data LiteralExpressionSyntax = LiteralExpressionSyntax
  { literalToken :: SyntaxToken
  }

literalExpressionSyntax :: SyntaxToken -> ExpressionSyntax
literalExpressionSyntax t = ExpressionSyntax $ LiteralExpressionSyntax t

instance IsSyntaxNode LiteralExpressionSyntax where
  kind _ = SyntaxKind.LiteralExpression
  children (LiteralExpressionSyntax tok) = [node tok]

instance IsExpressionSyntax LiteralExpressionSyntax
