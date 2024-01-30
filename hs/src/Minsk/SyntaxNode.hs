module Minsk.SyntaxNode
  ( IsSyntaxNode (..),
    SyntaxNode (..),
    pprint,
  )
where

import BasicPrelude
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Formatting (hprintLn, later, shown, text, (%))
import Minsk.SyntaxKind (SyntaxKind)
import System.IO (Handle)

data SyntaxNode = SyntaxNode
  { kind :: SyntaxKind,
    children :: [SyntaxNode],
    showEx :: TL.Builder
  }

class IsSyntaxNode n where
  node :: n -> SyntaxNode

pprint :: Handle -> SyntaxNode -> IO ()
pprint = pprint' "" True
  where
    pprint' :: TL.Text -> Bool -> Handle -> SyntaxNode -> IO ()
    pprint' indent isLast h n = do
      let marker = if isLast then "└── " else "├── "
      hprintLn h (text % text % shown % later showEx) indent marker (kind n) n
      let indent' = if isLast then indent <> "    " else indent <> "│   "
      let children' = children n
      let lastIndex = length children' - 1
      forM_ (zip [0 ..] children') \(i, child) -> do
        let isLast' = i == lastIndex
        pprint' indent' isLast' h child
