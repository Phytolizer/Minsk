open Types

type t = assignment

let make assignment_identifier_token assignment_equals_token
    assignment_expression =
  {
    assignment_identifier_token;
    assignment_equals_token;
    assignment_expression;
  }

let identifier_token x = x.assignment_identifier_token
let equals_token x = x.assignment_equals_token
let expression x = x.assignment_expression
