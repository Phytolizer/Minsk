module Minsk.Object (Object (..)) where

import BasicPrelude

data Object
  = String String
  | Number Integer
  deriving (Eq, Show)
