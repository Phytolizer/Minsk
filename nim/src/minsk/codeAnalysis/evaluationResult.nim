import minsk/minskObject

type
  EvaluationResult* = object
    case success*: bool
    of true:
      value*: MinskObject
    of false:
      diagnostics*: seq[string]

proc evaluationResultOk*(value: MinskObject): EvaluationResult =
  EvaluationResult(
    success: true,
    value: value,
  )

proc evaluationResultError*(diagnostics: seq[string]): EvaluationResult =
  EvaluationResult(
    success: false,
    diagnostics: diagnostics,
  )
