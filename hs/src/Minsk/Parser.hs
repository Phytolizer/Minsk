module Minsk.Parser (newParser, doParse, diagnostics) where

import BasicPrelude
import Control.Monad.State (State, get, modify)
import Formatting (sformat, shown, (%))
import qualified Minsk.AST as AST
import Minsk.Diagnostic (Diagnostic (Diagnostic))
import qualified Minsk.Lexer as Lexer
import Minsk.Pass (Pass (Parsed))
import Minsk.SyntaxFacts (Precedence (..), binaryOperatorPrecedence, unaryOperatorPrecedence)
import Minsk.SyntaxKind (SyntaxKind)
import qualified Minsk.SyntaxKind as SyntaxKind
import Minsk.SyntaxToken (SyntaxToken (SyntaxToken, kind, position))
import Safe (atMay)

type ParsedExpression = AST.ExpressionSyntax Parsed

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

parseExpression :: Precedence -> State Parser ParsedExpression
parseExpression startPrec = do
  unaryPrecedence <- unaryOperatorPrecedence . kind <$> current
  left <-
    if unaryPrecedence /= PZero && unaryPrecedence >= startPrec
      then do
        op <- nextToken
        operand <- parseExpression unaryPrecedence
        return $ AST.EUnary () $ AST.UnaryExpression op operand
      else parsePrimaryExpression
  parseExpression' startPrec left
  where
    parseExpression' :: Precedence -> ParsedExpression -> State Parser ParsedExpression
    parseExpression' parentPrec left = do
      prec <- binaryOperatorPrecedence . kind <$> current
      if prec == PZero || prec <= parentPrec
        then return left
        else do
          op <- nextToken
          right <- parseExpression prec
          parseExpression' parentPrec $ AST.EBinary () $ AST.BinaryExpression left op right

parsePrimaryExpression :: State Parser ParsedExpression
parsePrimaryExpression = do
  c <- current
  case c.kind of
    SyntaxKind.OpenParenthesisToken -> do
      left <- nextToken
      expr <- parseExpression PZero
      right <- match SyntaxKind.CloseParenthesisToken
      return $ AST.EParenthesized () $ AST.ParenthesizedExpression left expr right
    _ -> do
      tok <- match SyntaxKind.NumberToken
      return $ AST.ELiteral () $ AST.LiteralExpression tok

doParse :: State Parser (ParsedExpression, SyntaxToken)
doParse = do
  e <- parseExpression PZero
  eof <- match SyntaxKind.EndOfFileToken
  return (e, eof)

diagnostics :: Parser -> [Diagnostic]
diagnostics = reverse . _diagnostics
