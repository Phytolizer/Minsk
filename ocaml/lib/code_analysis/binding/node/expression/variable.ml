open Types

type t = variable

let make variable_variable = { variable_variable }
let variable x = x.variable_variable
