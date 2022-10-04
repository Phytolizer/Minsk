import binding/binder
import evaluationResult
import evaluator

import syntax/syntaxNode
from syntax/parser import SyntaxTree

type
  Compilation* = ref object of SyntaxNode
    syntax*: SyntaxTree

proc newCompilation*(syntax: SyntaxTree): Compilation =
  new(result)
  result.syntax = syntax

proc evaluate*(node: Compilation): EvaluationResult =
  var binder = newBinder()
  let boundExpression = binder.bindExpression(node.syntax.root)
  let diagnostics = node.syntax.diagnostics & binder.diagnostics
  if diagnostics.len > 0:
    return evaluationResultError(diagnostics)

  var evaluator = newEvaluator(boundExpression)
  let value = evaluator.evaluate()
  return evaluationResultOk(value)
