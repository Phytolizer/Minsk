module Minsk.SyntaxNode
  ( IsSyntaxNode (..),
    SyntaxNode (..),
    node,
    pprint,
  )
where

import BasicPrelude
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Formatting (hprintLn, later, shown, text, (%))
import Minsk.SyntaxKind (SyntaxKind)
import System.IO (Handle)

class IsSyntaxNode n where
  kind :: n -> SyntaxKind
  children :: n -> [SyntaxNode]
  showEx :: n -> TL.Builder
  default showEx :: n -> TL.Builder
  showEx _ = mempty

data SyntaxNode = forall n. (IsSyntaxNode n) => SyntaxNode n

node :: (IsSyntaxNode n) => n -> SyntaxNode
node = SyntaxNode

pprint :: Handle -> SyntaxNode -> IO ()
pprint = pprint' "" True
  where
    pprint' :: TL.Text -> Bool -> Handle -> SyntaxNode -> IO ()
    pprint' indent isLast h (SyntaxNode n) = do
      let marker = if isLast then "└── " else "├── "
      hprintLn h (text % text % shown % later showEx) indent marker (kind n) n
      let indent' = if isLast then indent <> "    " else indent <> "│   "
      let children' = children n
      let lastIndex = length children' - 1
      forM_ (zip [0 ..] children') \(i, child) -> do
        let isLast' = i == lastIndex
        pprint' indent' isLast' h child
