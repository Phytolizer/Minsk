open Runtime
module Expression = Expression

type kind = KindToken | KindExpression of Expression.kind
[@@deriving show { with_path = false }]

type t = Expression of Expression.t | Token of Token.t

let kind x =
  match x with
  | Expression e -> KindExpression (Expression.kind e)
  | Token _ -> KindToken

let children x =
  match x with
  | Token _ -> [||]
  | Expression e ->
      Expression.children
        ~xctor:(fun x -> Expression x)
        ~tctor:(fun t -> Token t)
        e

let rec pretty_print_inner ~indent ~is_last out x =
  let marker = if is_last then "└───" else "├───" in
  Printf.fprintf out "%s%s" indent marker;
  (match x with
  | Token { value; kind; _ } -> (
      output_string out (kind |> Token.show_kind);
      match value with
      | Some value ->
          output_char out ' ';
          output_string out @@ Value.show value
      | None -> ())
  | _ -> output_string out (kind x |> show_kind));
  output_char out '\n';
  let indent =
    String.concat "" [ indent; (if is_last then "    " else "│   ") ]
  in
  let children = children x in
  let n = Array.length children in
  Array.iteri
    (fun i child -> pretty_print_inner ~indent ~is_last:(i = n - 1) out child)
    children

let pretty_print = pretty_print_inner ~indent:"" ~is_last:true

let rec span x =
  match x with
  | Token t -> Token.span t
  | Expression e ->
      let children =
        Expression.children
          ~xctor:(fun x -> Expression x)
          ~tctor:(fun x -> Token x)
          e
      in
      let first = span children.(0) in
      let last = span children.(Array.length children - 1) in
      let open Text.Span in
      from_bounds first.start (fin last)
