module Vertex = struct
  (** A vertex. *)
  type 'v t = 'v

  let eq v v' = v == v'
end

module Edge = struct
  type ('v,'e) t = { label : 'e; source : 'v Vertex.t list; target : 'v Vertex.t list }

  let make l s t = { label = l; source = s; target = t }

  let label e = e.label

  let source e = e.source

  let target e = e.target

  let eq e e' = e == e'
end

module Graph = struct
  (** A graph. *)
  type ('v,'e) t = { vertices : 'v Vertex.t list; edges : ('v,'e) Edge.t list }

  let add_vertex g v  = { g with vertices = v :: g.vertices }

  let add_edge g e s t = { g with edges = (Edge.make e s t) :: g.edges }
end

(** Maps between graphs. *)
module Map = struct
  type ('v,'e) t = { vertices : ('v Vertex.t * 'v Vertex.t) list; edges : (('v,'e) Edge.t * ('v,'e) Edge.t) list }

  let add_vertex g v v' = { g with vertices = (v,v') :: g.vertices }

  let add_edge g e e' = { g with edges = (e,e') :: g.edges }
end

module Signature = struct
  type label = string

  (** A signature. *)
  type t = (label,label) Graph.t

  (* include (Graph : module type of Graph with type t := t) *)

  type vertex = label Vertex.t

  type edge = (label,label) Edge.t

  let add_vertex (s:t) (v:vertex) : t = Graph.add_vertex s v

  let add_edge (s:t) l (src:vertex list) (tgt:vertex list) : t = Graph.add_edge s l src tgt
end

module Term = struct
  (** A term on a signature. *)
  type 's t =
    {
      graph : (Signature.vertex, Signature.edge) Graph.t;
      source : Signature.vertex list;
      target : Signature.vertex list
    }

  (** Find an instance of the second term in the first one. *)
  let matchings t t' = ()
end
