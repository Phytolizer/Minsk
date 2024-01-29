module Minsk.Object (Object (..)) where

import BasicPrelude

data Object
  = String String
  | Number Int
  deriving (Eq, Show)
