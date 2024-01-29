module Minsk.SyntaxToken (SyntaxToken (..)) where

import BasicPrelude
import Data.Text (Text)
import Minsk.Object (Object)
import Minsk.SyntaxKind (SyntaxKind)

data SyntaxToken = SyntaxToken
  { kind :: SyntaxKind,
    position :: Int,
    text :: Text,
    value :: Maybe Object
  }
  deriving (Show)
