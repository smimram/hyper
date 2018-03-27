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

  let name v : string = label (label v)

  let to_string =
    let uid = UID.Named.create () in
    fun v ->
    let s = label (label v) in
    let n = UID.Named.get uid s v in
    s^"@"^string_of_int n

  let list_to_string vv =
    String.concat "," (List.map to_string vv)
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

  let to_string e = label (label e)

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

  let to_string g =
    let edges =
      List.map
        (fun e ->
          Edge.to_string e ^ " : " ^ Vertex.list_to_string (Edge.source e) ^ " -> " ^ Vertex.list_to_string (Edge.target e)
        ) (edges g)
    in
    (* TODO: isolated vertices *)
    String.concat "\n" edges


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

    let hasv g v = Fun.has g.vertices v

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
    let i1 = copy g1 in
    let i2 = copy g2 in
    (* let i1,i2 = if disjoint g1 g2 then Map.id g1, Map.id g2 else copy g1, copy g2 in *)
    let g1 = Map.target i1 in
    let g2 = Map.target i2 in
    let g =
      {
        vertices = (vertices g1)@(vertices g2);
        edges = (edges g1)@(edges g2)
      }
    in
    let i1 = { i1 with target = g } in
    let i2 = { i2 with target = g } in
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
(*
  module Vertex = struct
    type t = Signature.vertex Vertex.t

    let label = Vertex.label

    let name v = label (label v)

    let to_string v =
      let s = name v in
      let n = UID.Named.get uid s v in
      s^"@"^string_of_int n
  end
 *)

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

  (** String representation. *)
  let to_string f =
    let vertex v = Vertex.to_string v in
    let edge e = Edge.to_string e in
    let vertices vv = Vertex.list_to_string vv in
    let src = vertices (source f) in
    let tgt = vertices (target f) in
    (* TODO: display isolated vertices *)
    src ^ " ---> " ^ tgt ^ "\n" ^ Graph.to_string (graph f)

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
    (* Printf.printf "COMPOSE\n%s\nWITH\n%s\n%!" (to_string f) (to_string g); *)
    assert (List.length (target f) = List.length (source g));
    assert (List.for_all2 (fun v v' -> Vertex.label (Vertex.label v) = Vertex.label (Vertex.label v')) (target f) (source g));
    let r =
      let fr r v v' =
        let r = if Equiv.has r v then r else Equiv.add r v in
        let r = if Equiv.has r v' then r else Equiv.add r v' in
        (* Printf.printf "merge %s with %s\n\n%!" (Vertex.to_string v) (Vertex.to_string v'); *)
        Equiv.merge r v v'
      in
      List.fold_left2 fr Equiv.empty (target f) (source g)
    in
    let i1,i2 = Graph.coprod (graph f) (graph g) in
    (* Printf.printf "COPROD\n%s\n\n%!" (Graph.to_string (Graph.Map.target i1)); *)
    let r = Equiv.map (fun x -> if Graph.Map.hasv i1 x then Graph.Map.appv i1 x else Graph.Map.appv i2 x) r in
    let repr = Equiv.prepr r in
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
