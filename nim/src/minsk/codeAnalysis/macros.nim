import std/macros

macro loop*(body: untyped): untyped =
  ## A macro that expands to a loop that runs forever.
  ## Equivalent to 'while true: body'.
  result = newStmtList()
  result.add quote do:
    while true:
      `body`
