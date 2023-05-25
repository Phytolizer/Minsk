open Types

type t = literal

let make literal_value = { literal_value }
let value v = v.literal_value
