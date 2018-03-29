(** Graph plotting library. *)

open Stdlib

module G = Graph.Graph
module T = Graph.Term

module Options = struct
end

module Complex = struct
  include Complex

  let random () =
    let r () = Random.float 2. -. 1. in
    { re = r (); im = r () }
end

module Physics = struct
  type point =
    {
      vertex : Graph.Term.vertex;
      mass : float;
      pos : Complex.t;
      speed : Complex.t;
      fixed : bool
    }

  type spring =
    {
      source : Graph.Term.vertex;
      target : Graph.Term.vertex
    }

  (* let create ?(mass=1.) ?speed () = *)
    (* let speed = Option.default (Complex.random ()) speed in *)
    (* { mass; speed } *)
end

let plot t =
  let g = T.graph t in
  let vo = T.vorderv t in
  let ho = T.horderv t in
  let nodes = ref [] in
  let edges = ref [] in
  (* let () = *)
    (* let source = T.source t in *)
    (* let source = Array.of_list source in *)
    (* let n = Array.length source in *)
    (* Array.iteri (fun i v -> nodes := (v,(0.,float i /.(float (n-1)))) :: !nodes) source *)
  (* in *)
  (* let () = *)
    (* let source = T.source t in *)
    (* let source = Array.of_list source in *)
    (* let n = Array.length source in *)
    (* Array.iteri (fun i v -> nodes := (v,(0.,float i /.(float (n-1)))) :: !nodes) target *)
  (* in *)
  ()

let graphics t =
  Graphics.open_graph "";
  
