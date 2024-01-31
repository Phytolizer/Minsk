module Minsk.Evaluator (evaluate) where

import BasicPrelude
import Formatting (sformat, stext, (%))
import qualified Minsk.AST as AST
import Minsk.Object (Object (Number))
import Minsk.Pass (Pass (Parsed))
import qualified Minsk.SyntaxKind as SyntaxKind
import Minsk.SyntaxToken (SyntaxToken (..))

evaluate :: AST.ExpressionSyntax Parsed -> Either Text Integer
evaluate = evaluateExpression

evaluateExpression :: AST.ExpressionSyntax Parsed -> Either Text Integer
evaluateExpression (AST.EBinary _ e) = evaluateBinaryExpression e
evaluateExpression (AST.ELiteral _ e) = evaluateLiteralExpression e
evaluateExpression (AST.EParenthesized _ e) = evaluateParenthesizedExpression e

evaluateBinaryExpression :: AST.BinaryExpression Parsed -> Either Text Integer
evaluateBinaryExpression e = do
  left <- evaluateExpression (AST.left e)
  right <- evaluateExpression (AST.right e)
  case e.operatorToken.kind of
    SyntaxKind.PlusToken -> return $ left + right
    SyntaxKind.MinusToken -> return $ left - right
    SyntaxKind.StarToken -> return $ left * right
    SyntaxKind.SlashToken -> return $ left `div` right
    _ -> Left "Unexpected binary operator"

evaluateLiteralExpression :: AST.LiteralExpression Parsed -> Either Text Integer
evaluateLiteralExpression e = case e.literalToken.value of
  Just (Number n) -> return n
  _ -> Left $ sformat ("Unexpected literal: " % stext) e.literalToken.text

evaluateParenthesizedExpression :: AST.ParenthesizedExpression Parsed -> Either Text Integer
evaluateParenthesizedExpression e = evaluateExpression (AST.expression e)
