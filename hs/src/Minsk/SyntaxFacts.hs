module Minsk.SyntaxFacts
  ( Precedence (..),
    binaryOperatorPrecedence,
    unaryOperatorPrecedence,
  )
where

import BasicPrelude
import Minsk.SyntaxKind (SyntaxKind (..))

data Precedence = PZero | PTerm | PFactor | PUnary
  deriving (Eq, Ord)

binaryOperatorPrecedence :: SyntaxKind -> Precedence
binaryOperatorPrecedence k = case k of
  PlusToken -> PTerm
  MinusToken -> PTerm
  StarToken -> PFactor
  SlashToken -> PFactor
  _ -> PZero

unaryOperatorPrecedence :: SyntaxKind -> Precedence
unaryOperatorPrecedence k = case k of
  PlusToken -> PUnary
  MinusToken -> PUnary
  _ -> PZero
