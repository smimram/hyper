(** Main. *)

open Graph

module V = Vertex
module E = Edge
module S = Signature
module T = Term

let () =
  let s = S.empty in
  let a = V.make "a" in
  let s = S.addv s a in
  let m = E.make "m" [a;a] [a] in
  let s = S.adde s m in
  let i = T.id [a] in
  let m = T.generator m in
  let f = T.comp (T.tens m i) m in
  ()
