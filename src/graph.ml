open Stdlib

module Vertex = struct
  (** A vertex. *)
  type 'v t = { label : 'v }

  let label v = v.label

  let eq v v' = v == v'
end

module Edge = struct
  type ('v,'e) t = { label : 'e; source : 'v Vertex.t list; target : 'v Vertex.t list }

  let make l s t = { label = l; source = s; target = t }

  let label e = e.label

  let source e = e.source

  let target e = e.target

  let eq e e' = e == e'

  let map_vertices f e =
    {
      label = e.label;
      source = List.map f e.source;
      target = List.map f e.target
    }
end

module Graph = struct
  (** A graph. *)
  type ('v,'e) t =
    {
      vertices : 'v Vertex.t list;
      edges : ('v,'e) Edge.t list
    }

  let vertices g = g.vertices

  let edges g = g.edges

  let add_vertex g v  = { g with vertices = v :: g.vertices }

  let add_edge g e s t = { g with edges = (Edge.make e s t) :: g.edges }

  let map_vertices f g =
    {
      vertices = List.map f g.vertices;
      edges = List.map (Edge.map_vertices f) g.edges
    }

  let vertex_pred g v =
    assert (List.memq v (vertices g));
    List.filter (fun e -> List.memq v (Edge.target e)) (edges g)

  let vertex_succ g v =
    assert (List.memq v (vertices g));
    List.filter (fun e -> List.memq v (Edge.source e)) (edges g)

  let edge_pred g e = Edge.source e

  let edge_succ g e = Edge.target e

  (** Disjoint union. *)
  let coprod g1 g2 =
    assert (List.interq (vertices g1) (vertices g2) = []);
    assert (List.interq (edges g1) (edges g2) = []);
    {
      vertices = g1.vertices@g2.vertices;
      edges = g1.edges@g2.edges
    }
end

(** Maps between graphs. *)
module Map = struct
  type ('v,'e) t =
    {
      source : ('v,'e) Graph.t;
      target : ('v,'e) Graph.t;
      vertices : ('v Vertex.t * 'v Vertex.t) list;
      edges : (('v,'e) Edge.t * ('v,'e) Edge.t) list
    }

  let add_vertex g v v' = { g with vertices = (v,v') :: g.vertices }

  let add_edge g e e' = { g with edges = (e,e') :: g.edges }

  (** Pick an undefined vertex. *)
  let pick_vertex f =
    let rec aux = function
      | (v,_)::l -> if List.memq v (Graph.vertices f.source) then v else aux l
      | [] -> raise Not_found
    in
    aux f.vertices
end

module Signature = struct
  (** Labels in signatures. *)
  type label = string

  (** A signature. *)
  type t = (label,label) Graph.t

  (* include (Graph : module type of Graph with type t := t) *)

  (** A vertex in the signature. *)
  type vertex = label Vertex.t

  (** An edge in the signature. *)
  type edge = (label,label) Edge.t

  let add_vertex (s:t) (v:vertex) : t = Graph.add_vertex s v

  let add_edge (s:t) l (src:vertex list) (tgt:vertex list) : t = Graph.add_edge s l src tgt
end

module Term = struct
  (** A term on a signature. *)
  type 's t =
    {
      graph : (Signature.vertex, Signature.edge) Graph.t;
      source : (Signature.vertex Vertex.t) list;
      target : (Signature.vertex Vertex.t) list
    }

  let source f = f.source

  let target f = f.target

  let graph f = f.graph

  let map_vertices s f =
    {
      graph = Graph.map_vertices s (graph f);
      source = List.map s (source f);
      target = List.map s (target f);
    }

  let comp f g =
    assert (List.length (target f) = List.length (source g));
    assert (List.for_all2 (fun v v' -> Vertex.label v = Vertex.label v') (target f) (source g));
    let r =
      let fr r v v' =
        let r = if Equiv.has r v then r else Equiv.add r v in
        let r = if Equiv.has r v' then r else Equiv.add r v' in
        Equiv.merge r v v'
      in
      List.fold_left2 fr Equiv.empty (target f) (source g)
    in
    let graph = Graph.map_vertices (Equiv.repr r) (Graph.coprod (graph f) (graph g)) in
    {
      graph;
      source = source f;
      target = target g
    }

  (** Find an instance of the second term in the first one. *)
  (* let matchings t t' = *)
    (* let rec aux vv ee = *)
      (* if vv = [] && ee = [] then  *)
    (* in *)
end
