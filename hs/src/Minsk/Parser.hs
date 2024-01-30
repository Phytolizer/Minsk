module Minsk.Parser (newParser, doParse, diagnostics) where

import BasicPrelude
import Control.Monad.State (State, get, modify)
import Formatting (sformat, shown, (%))
import Minsk.Diagnostic (Diagnostic (Diagnostic))
import Minsk.ExpressionSyntax (ExpressionSyntax)
import Minsk.ExpressionSyntax.Binary (binaryExpressionSyntax)
import Minsk.ExpressionSyntax.Literal (literalExpressionSyntax)
import Minsk.ExpressionSyntax.Parenthesized (parenthesizedExpressionSyntax)
import qualified Minsk.Lexer as Lexer
import Minsk.SyntaxKind (SyntaxKind)
import qualified Minsk.SyntaxKind as SyntaxKind
import Minsk.SyntaxToken (SyntaxToken (SyntaxToken, kind, position))
import Safe (atMay)

data Parser = Parser
  { _tokens :: [SyntaxToken],
    _diagnostics :: [Diagnostic],
    _pos :: Int
  }

newParser :: Text -> Parser
newParser text = do
  let (tokens, diags) = Lexer.allTokens text
  let tokens' = filter (\tok -> tok.kind `notElem` [SyntaxKind.WhitespaceToken, SyntaxKind.BadToken]) tokens
  let diags' = reverse diags
  Parser tokens' diags' 0

peek :: Int -> State Parser SyntaxToken
peek n = do
  (Parser tokens _ pos) <- get
  let index = pos + n
  return $ fromMaybe (last tokens) $ atMay tokens index

current :: State Parser SyntaxToken
current = peek 0

moveNext :: State Parser ()
moveNext = modify \p -> p {_pos = p._pos + 1}

nextToken :: State Parser SyntaxToken
nextToken = do
  c <- current
  moveNext
  return c

report :: Text -> State Parser ()
report msg = modify \p -> p {_diagnostics = Diagnostic msg : p._diagnostics}

match :: SyntaxKind -> State Parser SyntaxToken
match k = do
  c <- current
  if c.kind == k
    then nextToken
    else do
      report $ sformat ("ERROR: Unexpected token <" % shown % ">, expected <" % shown % ">") c.kind k
      return $ SyntaxToken k c.position "" Nothing

parseExpression :: State Parser ExpressionSyntax
parseExpression = parseTerm

parseTerm :: State Parser ExpressionSyntax
parseTerm = do
  left <- parseFactor

  parseTerm' left
  where
    parseTerm' left = do
      op <- current
      let k = op.kind
      if k `elem` [SyntaxKind.PlusToken, SyntaxKind.MinusToken]
        then do
          moveNext
          right <- parseFactor
          parseTerm' $ binaryExpressionSyntax left op right
        else return left

parseFactor :: State Parser ExpressionSyntax
parseFactor = do
  left <- parsePrimaryExpression

  parseFactor' left
  where
    parseFactor' left = do
      op <- current
      let k = op.kind
      if k `elem` [SyntaxKind.StarToken, SyntaxKind.SlashToken]
        then do
          moveNext
          right <- parsePrimaryExpression
          parseFactor' $ binaryExpressionSyntax left op right
        else return left

parsePrimaryExpression :: State Parser ExpressionSyntax
parsePrimaryExpression = do
  c <- current
  case c.kind of
    SyntaxKind.OpenParenthesisToken -> do
      left <- nextToken
      expr <- parseExpression
      right <- match SyntaxKind.CloseParenthesisToken
      return $ parenthesizedExpressionSyntax left expr right
    _ -> do
      tok <- match SyntaxKind.NumberToken
      return $ literalExpressionSyntax tok

doParse :: State Parser (ExpressionSyntax, SyntaxToken)
doParse = do
  e <- parseExpression
  eof <- match SyntaxKind.EndOfFileToken
  return (e, eof)

diagnostics :: Parser -> [Diagnostic]
diagnostics = reverse . _diagnostics
