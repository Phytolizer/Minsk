module Minsk.SyntaxToken (SyntaxToken (..)) where

import BasicPrelude
import Formatting (bformat, optioned, shown, (%))
import Minsk.Object (Object)
import Minsk.SyntaxKind (SyntaxKind)
import Minsk.SyntaxNode (IsSyntaxNode (..), SyntaxNode (SyntaxNode))

data SyntaxToken = SyntaxToken
  { kind :: SyntaxKind,
    position :: Int,
    text :: Text,
    value :: Maybe Object
  }
  deriving (Show)

instance IsSyntaxNode SyntaxToken where
  node (SyntaxToken k _ _ v) = SyntaxNode k [] (bformat (" " % optioned shown) v)
