module Minsk.Diagnostic (Diagnostic (..)) where

import BasicPrelude
import Data.Text (unpack)

newtype Diagnostic = Diagnostic Text

instance Show Diagnostic where
  show (Diagnostic message) = unpack message
