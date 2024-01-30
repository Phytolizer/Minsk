module Minsk.SyntaxToken (SyntaxToken (SyntaxToken, position, text, value)) where

import BasicPrelude
import Minsk.Object (Object)
import Minsk.SyntaxKind (SyntaxKind)
import Minsk.SyntaxNode (IsSyntaxNode (..))

data SyntaxToken = SyntaxToken
  { _kind :: SyntaxKind,
    position :: Int,
    text :: Text,
    value :: Maybe Object
  }
  deriving (Show)

instance IsSyntaxNode SyntaxToken where
  kind = _kind
  children _ = []
