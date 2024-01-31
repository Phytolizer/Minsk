module Minsk.SyntaxTree (SyntaxTree (..), parse) where

import BasicPrelude
import Control.Monad.State (runState)
import qualified Minsk.AST as AST
import Minsk.Diagnostic (Diagnostic)
import Minsk.Parser (doParse, newParser)
import qualified Minsk.Parser as Parser
import Minsk.Pass (Pass (Parsed))
import Minsk.SyntaxToken (SyntaxToken)

data SyntaxTree = SyntaxTree
  { root :: AST.ExpressionSyntax Parsed,
    diagnostics :: [Diagnostic],
    eofToken :: SyntaxToken
  }

parse :: Text -> SyntaxTree
parse text = do
  let parser = newParser text
  let ((e, eof), parser') = runState doParse parser
  SyntaxTree e (Parser.diagnostics parser') eof
