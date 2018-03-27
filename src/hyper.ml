(** Main. *)

open Graph

module V = Vertex
module E = Edge
module S = Signature

let () =
  let s = S.empty in
  let a = V.make "a" in
  let s = S.addv s a in
  let m = E.make "m" [a;a] [a] in
  let s = S.adde s m in
  (* let f =  *)
  ()
