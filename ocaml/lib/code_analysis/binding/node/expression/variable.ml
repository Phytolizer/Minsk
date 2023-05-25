open Types

type t = variable

let make variable_name variable_ty = { variable_name; variable_ty }
let name x = x.variable_name
let ty x = x.variable_ty
