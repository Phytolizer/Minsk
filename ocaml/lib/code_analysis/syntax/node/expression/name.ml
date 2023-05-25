open Types

type t = name

let make name_identifier_token = { name_identifier_token }
let identifier_token x = x.name_identifier_token
