(** Main. *)

open Stdlib
open Graph

module V = Vertex
module E = Edge
module S = Signature
module T = Term
module R = Term.Rule

let () =
  let s = S.empty in
  let a = V.make "a" in
  let s = S.addv s a in
  let m = E.make "m" [a;a] [a] in
  let s = S.adde s m in
  let i = T.id [a] in
  let i2 = T.id [a;a] in
  let m = T.generator m in
  (*
  let to_string f =
    let uid = UID.Named.create () in
    let vertex v =
      let s = Vertex.label (Vertex.label v) in
      let n = UID.Named.get uid s v in
      s^"@"^string_of_int n
    in
    T.to_string ~vertex f
  in
   *)
  let assl = T.comp (T.tens m i) m in
  let assr = T.comp (T.tens i m) m in
  let ass = R.make assl assr in
  let f = T.comp (T.tens m i) m in
  let g = T.comp (T.tens m i2) f in
  Printf.printf "%s\n\n%!" (T.to_string assl);
  Printf.printf "%s\n\n%!" (T.to_string g);
  let m = T.matchings assl g in
  Printf.printf "%d matchings\n\n%!" (List.length m);
  Printf.printf "%s\n\n%!" (T.to_string (Option.get (R.rewrite ass g)))
