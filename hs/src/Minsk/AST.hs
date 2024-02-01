module Minsk.AST where

import BasicPrelude
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Formatting (bformat, hprintLn, later, optioned, shown, text, (%))
import Minsk.Pass (Pass (Parsed))
import Minsk.SyntaxKind (SyntaxKind)
import qualified Minsk.SyntaxKind as SyntaxKind
import Minsk.SyntaxToken (SyntaxToken (SyntaxToken))
import System.IO (Handle)

data Info = Info
  { kind :: SyntaxKind,
    children :: [Info],
    showEx :: TL.Builder
  }

data SyntaxNode (p :: Pass)
  = NExpression (XExpression p) (ExpressionSyntax p)
  | NToken SyntaxToken

data ExpressionSyntax (p :: Pass)
  = EBinary (XBinaryExpression p) (BinaryExpression p)
  | ELiteral (XLiteralExpression p) LiteralExpression
  | EParenthesized (XParenthesizedExpression p) (ParenthesizedExpression p)
  | EUnary (XUnaryExpression p) (UnaryExpression p)

data BinaryExpression (p :: Pass) = BinaryExpression
  { left :: ExpressionSyntax p,
    operatorToken :: SyntaxToken,
    right :: ExpressionSyntax p
  }

data LiteralExpression = LiteralExpression
  { literalToken :: SyntaxToken
  }

data ParenthesizedExpression (p :: Pass) = ParenthesizedExpression
  { openParenthesisToken :: SyntaxToken,
    expression :: ExpressionSyntax p,
    closeParenthesisToken :: SyntaxToken
  }

data UnaryExpression (p :: Pass) = UnaryExpression
  { operatorToken :: SyntaxToken,
    operand :: ExpressionSyntax p
  }

type family XExpression (p :: Pass)

type instance XExpression Parsed = ()

type family XBinaryExpression (p :: Pass)

type instance XBinaryExpression Parsed = ()

type family XLiteralExpression (p :: Pass)

type instance XLiteralExpression Parsed = ()

type family XParenthesizedExpression (p :: Pass)

type instance XParenthesizedExpression Parsed = ()

type family XUnaryExpression (p :: Pass)

type instance XUnaryExpression Parsed = ()

info :: SyntaxNode p -> Info
info (NExpression _ e) = expInfo e
info (NToken t) = tokenInfo t

expInfo :: ExpressionSyntax p -> Info
expInfo (EBinary _ e) = binaryInfo e
expInfo (ELiteral _ e) = literalInfo e
expInfo (EParenthesized _ e) = parenthesizedInfo e
expInfo (EUnary _ e) = unaryInfo e

binaryInfo :: BinaryExpression p -> Info
binaryInfo (BinaryExpression left tok right) =
  Info
    { kind = SyntaxKind.BinaryExpression,
      children = [expInfo left, tokenInfo tok, expInfo right],
      showEx = mempty
    }

literalInfo :: LiteralExpression -> Info
literalInfo (LiteralExpression tok) =
  Info
    { kind = SyntaxKind.LiteralExpression,
      children = [tokenInfo tok],
      showEx = mempty
    }

parenthesizedInfo :: ParenthesizedExpression p -> Info
parenthesizedInfo (ParenthesizedExpression open e close) =
  Info
    { kind = SyntaxKind.ParenthesizedExpression,
      children = [tokenInfo open, expInfo e, tokenInfo close],
      showEx = mempty
    }

unaryInfo :: UnaryExpression p -> Info
unaryInfo (UnaryExpression tok e) =
  Info
    { kind = SyntaxKind.UnaryExpression,
      children = [tokenInfo tok, expInfo e],
      showEx = mempty
    }

tokenInfo :: SyntaxToken -> Info
tokenInfo (SyntaxToken k _ _ v) =
  Info
    { kind = k,
      children = [],
      showEx = bformat (" " % optioned shown) v
    }

pprint :: Handle -> SyntaxNode p -> IO ()
pprint handle node = pprint' "" True handle (info node)
  where
    pprint' :: TL.Text -> Bool -> Handle -> Info -> IO ()
    pprint' indent isLast h n = do
      let marker = if isLast then "└── " else "├── "
      hprintLn h (text % text % shown % later showEx) indent marker (kind n) n
      let indent' = if isLast then indent <> "    " else indent <> "│   "
      let children' = children n
      let lastIndex = length children' - 1
      forM_ (zip [0 ..] children') \(i, child) -> do
        let isLast' = i == lastIndex
        pprint' indent' isLast' h child
