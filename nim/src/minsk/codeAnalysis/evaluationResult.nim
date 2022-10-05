import minsk/codeAnalysis/diagnostic
import minsk/minskObject

type
  EvaluationResult* = object
    case success*: bool
    of true:
      value*: MinskObject
    of false:
      diagnostics*: seq[Diagnostic]

proc evaluationResultOk*(value: MinskObject): EvaluationResult =
  EvaluationResult(
    success: true,
    value: value,
  )

proc evaluationResultError*(diagnostics: seq[Diagnostic]): EvaluationResult =
  EvaluationResult(
    success: false,
    diagnostics: diagnostics,
  )
