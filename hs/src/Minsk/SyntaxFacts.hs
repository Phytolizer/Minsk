module Minsk.SyntaxFacts
  ( Precedence (..),
    binaryOperatorPrecedence,
  )
where

import BasicPrelude
import Minsk.SyntaxKind (SyntaxKind (..))

data Precedence = PZero | PTerm | PFactor
  deriving (Eq, Ord)

binaryOperatorPrecedence :: SyntaxKind -> Precedence
binaryOperatorPrecedence k = case k of
  PlusToken -> PTerm
  MinusToken -> PTerm
  StarToken -> PFactor
  SlashToken -> PFactor
  _ -> PZero
