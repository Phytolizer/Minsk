module Minsk.Object (Object (..)) where

data Object
    = String String
    | Number Int
    deriving (Eq, Show)
