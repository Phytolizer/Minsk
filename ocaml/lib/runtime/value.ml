type t = Int of int | Bool of bool
type ty = TyInt | TyBool

let show_ty = function TyInt -> "Int" | TyBool -> "Bool"
let tyof = function Int _ -> TyInt | Bool _ -> TyBool
let get_int = function Int i -> i | _ -> failwith "not int"
let get_bool = function Bool b -> b | _ -> failwith "not bool"

let show out = function
  | Int i -> Printf.fprintf out "%d" i
  | Bool b -> Printf.fprintf out "%B" b
