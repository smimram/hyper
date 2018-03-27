(** Hypergraphs. *)

open Stdlib
module List = Listq

(** Labeled vertices. *)
module Vertex = struct
  (** A vertex. *)
  type 'v t = { label : 'v }

  (** Label. *)
  let label v = v.label

  (** Equality *)
  let eq v v' = v == v'

  (** Create a vertex with given label. *)
  let make label = { label }

  (** Fresh copy of a vertex. *)
  let copy v = make (label v)
end

(** Labeled edges. *)
module Edge = struct
  (** An edge. *)
  type ('v,'e) t = { label : 'e; source : 'v Vertex.t list; target : 'v Vertex.t list }

  (** Create an edge. *)
  let make l s t = { label = l; source = s; target = t }

  (** Label. *)
  let label e = e.label

  (** Source. *)
  let source e = e.source

  (** Target. *)
  let target e = e.target

  (** Equality. *)
  let eq e e' = e == e'

  (** Map a function on vertices. *)
  let map f e =
    {
      label = e.label;
      source = List.map f e.source;
      target = List.map f e.target
    }
end

(** Hypergraphs. *)
module Graph = struct
  (** A graph. *)
  type ('v,'e) t =
    {
      vertices : 'v Vertex.t list;
      edges : ('v,'e) Edge.t list
    }
  type ('v,'e) graph = ('v,'e) t

  let vertices g = g.vertices

  let edges g = g.edges

  let make vertices edges = { vertices; edges }

  (** Empty graph. *)
  let empty = make [] []

  (** Discrete graph with given set of vertices. *)
  let discrete u = make u []

  let addv g v  = { g with vertices = v :: g.vertices }

  let adde g e = { g with edges = e :: g.edges }

  let map (fv,fe) g =
    {
      vertices = List.unique (List.map fv g.vertices);
      edges = List.unique (List.map fe g.edges)
    }

  let vertex_pred g v =
    assert (List.mem v (vertices g));
    List.filter (fun e -> List.mem v (Edge.target e)) (edges g)

  let vertex_succ g v =
    assert (List.mem v (vertices g));
    List.filter (fun e -> List.mem v (Edge.source e)) (edges g)

  let edge_pred g e = Edge.source e

  let edge_succ g e = Edge.target e

  (** Partial maps between graphs. *)
  module Map = struct
    type ('v,'e) t =
      {
        source : ('v,'e) graph;
        target : ('v,'e) graph;
        vertices : 'v Vertex.t Fun.t;
        edges : ('v,'e) Edge.t Fun.t
      }

    let source f = f.source

    let target f = f.target

    (* (\** Empty map. *\) *)
    (* let empty source target = { source; target; vertices = []; edges = [] } *)

    (** Identity map. *)
    let id g =
      {
        source = g;
        target = g;
        vertices = List.fold_left (fun f x -> Fun.add f x x) Fun.empty (vertices g);
        edges = List.fold_left (fun f e -> Fun.add f e e) Fun.empty (edges g)
      }

    let appv g v = Fun.app g.vertices v

    let appe g e = Fun.app g.edges e

    let addv g v v' = { g with vertices = Fun.add g.vertices v v' }

    let adde g e e' = { g with edges = Fun.add g.edges e e' }

    (** Pick an undefined vertex. *)
    let pickv f =
      let rec aux = function
        | (v,_)::l -> if List.mem v (vertices f.source) then v else aux l
        | [] -> raise Not_found
      in
      aux f.vertices

    (** Sequential composition. *)
    let comp f g =
      {
        source = source f;
        target = target g;
        vertices = Fun.comp f.vertices g.vertices;
        edges = Fun.comp f.edges g.edges
      }
  end

  (** Replace vertices (and edges) with fresh ones. *)
  let copy g =
    let fv = ref Fun.empty in
    let fe = ref Fun.empty in
    let funv v =
      try
        Fun.app !fv v
      with
      | Not_found ->
         let v' = Vertex.copy v in
         fv := Fun.add !fv v v';
         v'
    in
    let fune e =
      try
        Fun.app !fe e
      with
      | Not_found ->
         let e' = Edge.map funv e in
         fe := Fun.add !fe e e';
         e'
    in
    let g' = map (funv,fune) g in
    { Map.
      source = g;
      target = g';
      vertices = !fv;
      edges = !fe
    }

  (** Quotient of a graph. *)
  let quotient g s =
    let fv = List.fold_left (fun f x -> Fun.add f x (s x)) Fun.empty (vertices g) in
    let fe = List.fold_left (fun f e -> Fun.add f e (Edge.map (Fun.app fv) e)) Fun.empty (edges g) in
    let g' =
      let fv = Fun.app fv in
      let fe = Fun.app fe in
      map (fv,fe) g
    in
    { Map.
      source = g;
      target = g';
      vertices = fv;
      edges = fe
    }

  (** Test whether two graphs are disjoint. *)
  let disjoint g1 g2 =
    List.inter (vertices g1) (vertices g2) = [] && List.inter (edges g1) (edges g2) = []

  (** Disjoint union. *)
  let coprod g1 g2 =
    let g1 = copy g1 in
    let g2 = copy g2 in
    let g =
      {
        vertices = (vertices (Map.target g1))@(vertices (Map.target g2));
        edges = (edges (Map.target g1))@(edges (Map.target g2))
      }
    in
    let i1 = { g1 with target = g } in
    let i2 = { g2 with target = g } in
    i1, i2
end

(** Signatures. *)
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

  (** Empty signature. *)
  let empty : t = Graph.empty

  (** Add a vertex. *)
  let addv (s:t) (v:vertex) : t = Graph.addv s v

  (** Add an edge. *)
  let adde (s:t) e : t = Graph.adde s e
end

                 
(** Terms. *)
module Term = struct
  (** A term on a signature. *)
  type t =
    {
      graph : (Signature.vertex, Signature.edge) Graph.t;
      source : (Signature.vertex Vertex.t) list;
      target : (Signature.vertex Vertex.t) list
    }

  let source f = f.source

  let target f = f.target

  let graph f = f.graph

  let make graph source target = { graph; source; target }

  (*
  (** Morphisms between terms. *)
  module Map = struct
    type t =
      {
        graph : (Signature.vertex, Signature.edge) Graph.Map.t;
        source : (Signature.vertex Vertex.t) list;
        target : (Signature.vertex Vertex.t) list
      }

    let source f =
      make (Graph.Map.source f.graph) f.source f.target

    let target f =
      make (Graph.Map.target f.graph) f.source f.target
  end
  *)

  (** Identity. *)
  let id u =
    let u = List.map Vertex.make u in
    {
      graph = Graph.discrete u;
      source = u;
      target = u;
    }

  (** Generating term. *)
  let generator e =
    let source = List.map Vertex.make (Edge.source e) in
    let target = List.map Vertex.make (Edge.target e) in
    let edge = Edge.make e source target in
    let graph = Graph.make (source@target) [edge] in
    make graph source target

  (** Copose two terms. *)
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
    let repr = Equiv.prepr r in
    let i1,i2 = Graph.coprod (graph f) (graph g) in
    let i = Graph.quotient (Graph.Map.target i1) repr in
    let i1 = Graph.Map.comp i1 i in
    let i2 = Graph.Map.comp i2 i in
    (* TODO: morphism *)
    {
      graph = Graph.Map.target i;
      source = List.map (Graph.Map.appv i1) (source f);
      target = List.map (Graph.Map.appv i2) (target g)
    }

  (** Tensor product. *)
  let tens g1 g2 =
    let i1,i2 = Graph.coprod (graph g1) (graph g2) in
    let f1 = Graph.Map.appv i1 in
    let f2 = Graph.Map.appv i2 in
    let graph = Graph.Map.target i1 in
    (* TODO: morphism *)
    {
      graph;
      source = (List.map f1 (source g1))@(List.map f2 (source g2));
      target = (List.map f1 (target g1))@(List.map f2 (target g2))
    }

  (** Find an instance of the second term in the first one. *)
  (* let matchings t t' = *)
    (* let rec aux vv ee = *)
      (* if vv = [] && ee = [] then  *)
    (* in *)
end
