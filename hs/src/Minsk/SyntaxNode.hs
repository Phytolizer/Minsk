module Minsk.SyntaxNode
  ( IsSyntaxNode (..),
    SyntaxNode (..),
    node,
  )
where

import Minsk.SyntaxKind (SyntaxKind)

class IsSyntaxNode n where
  kind :: n -> SyntaxKind
  children :: n -> [SyntaxNode]

data SyntaxNode = forall n. (IsSyntaxNode n) => SyntaxNode n

node :: (IsSyntaxNode n) => n -> SyntaxNode
node = SyntaxNode
