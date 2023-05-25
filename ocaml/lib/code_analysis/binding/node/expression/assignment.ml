open Types

type t = assignment

let make assignment_name assignment_expression =
  { assignment_name; assignment_expression }

let name x = x.assignment_name
let expression x = x.assignment_expression
