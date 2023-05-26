open Types

type t = literal

let make literal_literal_token literal_value =
  { literal_literal_token; literal_value }

let literal_token x = x.literal_literal_token
let value x = x.literal_value
