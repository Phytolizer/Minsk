module Minsk.Object (Object (..)) where

import BasicPrelude

data Object
  = String String
  | Number Integer
  deriving (Eq)

instance Show Object where
  show (String s) = s
  show (Number n) = show n
