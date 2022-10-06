import std/tables

import minsk/minskObject

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

proc evaluate*(
  node: Compilation,
  variables: TableRef[string, MinskObject]
): EvaluationResult =
  var binder = newBinder(variables)
  var diagnostics = node.syntax.diagnostics
  if diagnostics.len > 0:
    return evaluationResultError(diagnostics)
  let boundExpression = binder.bindExpression(node.syntax.root)
  diagnostics = binder.diagnostics
  if diagnostics.len > 0:
    return evaluationResultError(diagnostics)

  var evaluator = newEvaluator(boundExpression, variables)
  let value = evaluator.evaluate()
  return evaluationResultOk(value)
