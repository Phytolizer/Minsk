module Minsk.SyntaxKind (SyntaxKind (..)) where

import BasicPrelude

data SyntaxKind
  = -- Special tokens
    BadToken
  | EndOfFileToken
  | WhitespaceToken
  | -- Single-char tokens
    PlusToken
  | MinusToken
  | StarToken
  | SlashToken
  | OpenParenthesisToken
  | CloseParenthesisToken
  | -- Multi-char tokens
    NumberToken
  | -- Expressions
    BinaryExpression
  | LiteralExpression
  | ParenthesizedExpression
  | UnaryExpression
  deriving (Eq, Show)
