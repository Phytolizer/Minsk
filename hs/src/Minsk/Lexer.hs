module Minsk.Lexer (allTokens) where

import Control.Monad.State (
    State,
    evalState,
    get,
    put,
 )
import Data.Text (Text, uncons)
import qualified Data.Text as Text
import Minsk.SyntaxKind (SyntaxKind)
import qualified Minsk.SyntaxKind as SyntaxKind
import Minsk.SyntaxToken (SyntaxToken (..))

data Lexer = Lexer
    { _text :: Text
    , _pos :: Int
    , _start :: Int
    , _tokenText :: String
    , _current :: Maybe Char
    }

new :: Text -> Lexer
new text =
    Lexer
        { _text = text
        , _pos = 0
        , _start = 0
        , _tokenText = []
        , _current = Nothing
        }

current :: State Lexer Char
-- Get current char, with buffering.
current = do
    lexer <- get
    case _current lexer of
        Just c -> return c
        Nothing -> case uncons (_text lexer) of
            Just (c, _) -> do
                put lexer{_current = Just c}
                return c
            Nothing -> return '\0'

advance :: State Lexer ()
-- Advance to next char.
advance = do
    lexer <- get
    case _current lexer of
        Just _ -> put lexer{_current = Nothing, _pos = _pos lexer + 1}
        Nothing -> case uncons (_text lexer) of
            Just (_, text') -> put lexer{_text = text', _pos = _pos lexer + 1}
            Nothing -> return ()

startToken :: State Lexer ()
startToken = do
    lexer <- get
    put lexer{_start = _pos lexer, _tokenText = []}

mkToken :: Lexer -> SyntaxKind -> SyntaxToken
mkToken lexer kind =
    SyntaxToken
        { kind = kind
        , position = _start lexer
        , text = Text.pack $ _tokenText lexer
        , value = Nothing
        }

nextToken :: State Lexer SyntaxToken
-- Yield next token.
nextToken = do
    lexer <- get
    startToken
    c <- current
    case c of
        '\0' -> do
            lexer' <- get
            return $ mkToken lexer' SyntaxKind.EndOfFileToken
        c -> do
            -- TODO: handle more characters.
            advance
            lexer' <- get
            return $ mkToken lexer' SyntaxKind.BadToken

isEndOfFileToken :: SyntaxToken -> Bool
isEndOfFileToken token = kind token == SyntaxKind.EndOfFileToken

allTokens :: Text -> [SyntaxToken]
allTokens text = evalState go (new text)
  where
    go :: State Lexer [SyntaxToken]
    go = do
        token <- nextToken
        case kind token of
            SyntaxKind.EndOfFileToken -> return [token]
            _ -> do
                tokens <- go
                return $ token : tokens
