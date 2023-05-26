open Alcotest
module Syntax = Minsk.Code_analysis.Syntax

type t = { mutable it : Syntax.Node.t Seq.t }

let flatten root =
  let result = ref Seq.empty in
  let stack = ref (BatVect.singleton root) in
  while BatVect.length !stack > 0 do
    let n, stack' = BatVect.pop !stack in
    stack := stack';
    result := Seq.append !result (Seq.return n);
    Syntax.Node.children n |> Array.to_list |> List.rev
    |> List.iter (fun child -> stack := BatVect.append child !stack)
  done;
  !result

let make root = { it = flatten root }

let assert_node ~fmt ~kind self =
  match Seq.uncons self.it with
  | None -> fail (fmt "not enough nodes")
  | Some (n, rest) ->
      self.it <- rest;
      (check Node_ext.kind) (fmt "correct kind") kind (Syntax.Node.kind n)

let assert_token ~fmt ~kind ~text self =
  match Seq.uncons self.it with
  | None -> fail (fmt "not enough nodes")
  | Some (n, rest) -> (
      self.it <- rest;
      match n with
      | Syntax.Node.Token tok ->
          (check Token_ext.kind) (fmt "correct kind") kind tok.kind;
          (check string) (fmt "correct text") text tok.text
      | _ ->
          (check Node_ext.kind) (fmt "is a token") KindToken
            (Syntax.Node.kind n))

let finish ~fmt self =
  match Seq.uncons self.it with
  | None -> ()
  | Some _ -> fail (fmt "trailing nodes!")
