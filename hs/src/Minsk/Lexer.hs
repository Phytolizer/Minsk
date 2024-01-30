module Minsk.Lexer (allTokens) where

import BasicPrelude hiding (takeWhile, uncons)
import Control.Monad.State
  ( State,
    evalState,
    get,
    gets,
    modify,
    put,
  )
import Data.Text (uncons)
import qualified Data.Text as Text
import Formatting (char, sformat, (%))
import GHC.Unicode (isDigit, isSpace)
import Minsk.Diagnostic (Diagnostic (Diagnostic))
import Minsk.Object (Object (Number))
import Minsk.SyntaxKind (SyntaxKind)
import qualified Minsk.SyntaxKind as SyntaxKind
import Minsk.SyntaxToken (SyntaxToken (SyntaxToken, kind))

data Lexer = Lexer
  { _text :: Text,
    _pos :: Int,
    _start :: Int,
    _tokenText :: String,
    _current :: Maybe Char,
    _diagnostics :: [Diagnostic]
  }

new :: Text -> Lexer
new text =
  Lexer
    { _text = text,
      _pos = 0,
      _start = 0,
      _tokenText = [],
      _current = Nothing,
      _diagnostics = []
    }

current :: State Lexer Char
-- Get current char, with buffering.
current = do
  lexer <- get
  case _current lexer of
    Just c -> return c
    Nothing -> case uncons (_text lexer) of
      Just (c, text') -> do
        put lexer {_current = Just c, _text = text'}
        return c
      Nothing -> return '\0'

advance :: State Lexer ()
-- Advance to next char.
advance = do
  lexer <- get
  case _current lexer of
    Just c ->
      put
        lexer
          { _current = Nothing,
            _pos = _pos lexer + 1,
            _tokenText = c : _tokenText lexer
          }
    Nothing -> case uncons (_text lexer) of
      Just (c, text') ->
        put
          lexer
            { _text = text',
              _pos = _pos lexer + 1,
              _tokenText = c : _tokenText lexer
            }
      Nothing -> return ()

tokenText :: State Lexer Text
tokenText = gets (Text.pack . reverse . _tokenText)

startToken :: State Lexer ()
startToken = do
  lexer <- get
  put lexer {_start = _pos lexer, _tokenText = []}

mkTokenInner :: SyntaxKind -> Maybe Object -> State Lexer SyntaxToken
mkTokenInner k value = do
  start <- gets _start
  text <- tokenText
  return $ SyntaxToken k start text value

mkToken :: SyntaxKind -> State Lexer SyntaxToken
mkToken k = mkTokenInner k Nothing

mkToken' :: SyntaxKind -> Object -> State Lexer SyntaxToken
mkToken' k value = mkTokenInner k (Just value)

takeWhile :: (Char -> Bool) -> State Lexer ()
takeWhile p = do
  c <- current
  when (p c) do
    advance
    takeWhile p

report :: Text -> State Lexer ()
report msg = modify \lexer -> lexer {_diagnostics = Diagnostic msg : _diagnostics lexer}

nextToken :: State Lexer SyntaxToken
-- Yield next token.
nextToken = do
  startToken
  c <- current
  if
    | c == '\0' -> do
        mkToken SyntaxKind.EndOfFileToken
    | c `elem` ['+', '-', '*', '/', '(', ')'] -> do
        advance
        mkToken $ case c of
          '+' -> SyntaxKind.PlusToken
          '-' -> SyntaxKind.MinusToken
          '*' -> SyntaxKind.StarToken
          '/' -> SyntaxKind.SlashToken
          '(' -> SyntaxKind.OpenParenthesisToken
          ')' -> SyntaxKind.CloseParenthesisToken
          _ -> undefined
    | isDigit c -> do
        takeWhile isDigit
        value <- fromMaybe 0 . readMay <$> tokenText
        mkToken' SyntaxKind.NumberToken (Number value)
    | isSpace c -> do
        takeWhile isSpace
        mkToken SyntaxKind.WhitespaceToken
    | otherwise -> do
        advance
        report $ sformat ("ERROR: Bad character in input: '" % char % "'") c
        mkToken SyntaxKind.BadToken

allTokens :: Text -> ([SyntaxToken], [Diagnostic])
allTokens text = evalState go (new text)
  where
    go :: State Lexer ([SyntaxToken], [Diagnostic])
    go = do
      token <- nextToken
      case token.kind of
        SyntaxKind.EndOfFileToken -> do
          diagnostics <- gets (reverse . _diagnostics)
          return ([token], diagnostics)
        _ -> do
          (tokens, diagnostics) <- go
          return (token : tokens, diagnostics)
