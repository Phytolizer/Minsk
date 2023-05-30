type t = { start : int; length : int } [@@deriving show]

let make start length = { start; length }
let from_bounds start fin = make start (fin - start)
let fin x = x.start + x.length
