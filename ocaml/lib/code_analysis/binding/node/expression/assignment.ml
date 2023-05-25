open Types

type t = assignment

let make assignment_variable assignment_expression =
  { assignment_variable; assignment_expression }

let variable x = x.assignment_variable
let expression x = x.assignment_expression
