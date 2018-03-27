(** Main. *)

open Graph

module V = Vertex
module E = Edge
module S = Signature

let () =
  let s = S.empty in
  let a = V.make "a" in
  let s = S.add_vertex s in
  ()
