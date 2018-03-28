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

  let hasv g x = List.mem x (vertices g)

  let hase g e = List.mem e (edges g)

  let addv g x  = { g with vertices = x :: g.vertices }

  let adde g e = { g with edges = e :: g.edges }

  let map (fv,fe) g =
    {
      vertices = List.unique (List.map fv g.vertices);
      edges = List.unique (List.map fe g.edges)
    }

  (** Predecessors of a vertex. *)
  let vertex_pred g x =
    assert (hasv g x);
    List.filter (fun e -> List.mem x (Edge.target e)) (edges g)

  (** Successors of a vertex. *)
  let vertex_succ g x =
    assert (hasv g x);
    List.filter (fun e -> List.mem x (Edge.source e)) (edges g)

  (** Predecessors of an edge. *)
  let edge_pred g e = Edge.source e

  (** Successors of an edge. *)
  let edge_succ g e = Edge.target e

  (** Partial maps between graphs. *)
  module Map = struct
    (** A map. *)
    type ('v,'e) t =
      {
        source : ('v,'e) graph;
        target : ('v,'e) graph;
        vertices : 'v Vertex.t Fun.t;
        edges : ('v,'e) Edge.t Fun.t
      }

    let to_string f =
      String.concat " " (List.map (fun (x,x') -> Vertex.to_string x ^ " -> " ^ Vertex.to_string x') f.vertices) ^ "\n"
      ^ String.concat " " (List.map (fun (e,e') -> Edge.to_string e ^ " -> " ^ Edge.to_string e') f.edges) ^ "\n"

    (** Source. *)
    let source f = f.source

    (** Target. *)
    let target f = f.target

    (** Empty map. *)
    let empty source target = { source; target; vertices = []; edges = [] }

    (** Identity map. *)
    let id g =
      {
        source = g;
        target = g;
        vertices = List.fold_left (fun f x -> Fun.add f x x) Fun.empty (vertices g);
        edges = List.fold_left (fun f e -> Fun.add f e e) Fun.empty (edges g)
      }

    (** Whether the map is defined on a given vertex. *)
    let hasv f x = Fun.has f.vertices x

    (** Whether the map is defined on a given edge. *)
    let hase f e = Fun.has f.edges e

    let cohasv f x = Fun.cohas f.vertices x

    let cohase f e = Fun.cohas f.edges e

    (** Image of a vertex. *)
    let appv f x = Fun.app f.vertices x

    (** Image of an edge. *)
    let appe f e = Fun.app f.edges e

    (** Add image for a vertex. *)
    let addv f x x' = { f with vertices = Fun.add f.vertices x x' }

    (** Add image for edge. *)
    let adde f e e' = { f with edges = Fun.add f.edges e e' }

    (** Domain in vertices. *)
    let domv f =
      Fun.dom f.vertices

    (** Domain in edges. *)
    let dome f =
      Fun.dom f.edges

    (** Pick an undefined vertex. *)
    let pickv f =
      let l = List.sub (vertices f.source) (domv f) in
      if l = [] then raise Not_found else List.hd l

    (** Pick an undefined edge. *)
    let picke f =
      let l = List.sub (edges f.source) (dome f) in
      if l = [] then raise Not_found else List.hd l

    (** Whether the function is total on vertices. *)
    let totalv f =
      not (is_found pickv f)

    (** Whether the function is total on edges. *)
    let totale f =
      not (is_found picke f)

    let total f =
      totalv f && totale f

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

  (** Quotient on vertices of a graph. *)
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

  (** Remove vertices and edges from graph. *)
  let remove g rv re =
    {
      vertices = List.sub g.vertices rv;
      edges = List.sub g.edges re
    }

  (** Vertical order on vertices. *)
  let vorderv g =
    let p = ref (Poset.create (vertices g)) in
    List.iter
      (fun e ->
        List.iter_pairs
          (fun x y ->
            p := Poset.add !p x y
          ) (Edge.source e) (Edge.target e)
      ) (edges g);
    !p
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

  (** Vertices. *)
  let vertices (s:t) : vertex list = Graph.vertices s

  (** Edges. *)
  let edges (s:t) : edge list = Graph.edges s

  (** Empty signature. *)
  let empty : t = Graph.empty

  (** Add a vertex. *)
  let addv (s:t) (v:vertex) : t = Graph.addv s v

  (** Add an edge. *)
  let adde (s:t) (e:edge) : t = Graph.adde s e
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

  (** Vertex of a term. *)
  type vertex = Signature.vertex Vertex.t

  (** Underlying graph of a term. *)
  type graph = (Signature.vertex, Signature.edge) Graph.t

  (** A term on a signature. *)
  type t =
    {
      graph : graph; (** underlying graph *)
      source : vertex list; (** source *)
      target : vertex list (** target *)
    }

  type term = t

  (** Source. *)
  let source t = t.source

  (** Target. *)
  let target t = t.target

  (** Underlying graph. *)
  let graph t = t.graph

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

  (** Test whether two terms have the same source, and the same target. *)
  let parallel t1 t2 =
    let s1 = source t1 in
    let s2 = source t2 in
    let t1 = target t1 in
    let t2 = target t2 in
    List.length s1 = List.length s2
    && List.length t1 = List.length t2
    && List.for_all2 (fun x y -> Vertex.label x == Vertex.label y) s1 s2
    && List.for_all2 (fun x y -> Vertex.label x == Vertex.label y) t1 t2

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
    assert (List.for_all2 (fun v v' -> Vertex.label v == Vertex.label v') (target f) (source g));
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

  (** Find an instance of the first term in the second one. *)
  let matchings ?(injective=true) ?(convex=true) t t' =
    (* Printf.printf "MATCH\n%s\nWTIH\n%s\n\n%!" (to_string t) (to_string t'); *)
    let g = graph t in
    let g' = graph t' in
    let ans = ref [] in
    let return i =
      (* Check for convexity *)
      let is_convex i =
        let t' = Graph.Map.target i in
        let p = Graph.vorderv (graph t) in
        List.for_all_pairs (fun x y -> not (Poset.lt p y x)) (source t) (target t)
        (* List.iter_pairs (fun x y -> if Poset.lt p y x then Printf.printf "LOOP: %s < %s\n\n%!" (Vertex.to_string y) (Vertex.to_string x)) (source t) (target t); true *)
      in
      if not convex || is_convex i then
        ans := i :: !ans
    in
    let queue = Queue.create () in
    Queue.push ([], Graph.Map.empty g g') queue;
    while not (Queue.is_empty queue) do
      (* Printf.printf "loop\n"; *)
      try
        let l,i = Queue.pop queue in
        match l with
        | [] ->
           if not (Graph.Map.totalv i) then
             let x = Graph.Map.pickv i in
             List.iter (fun x' -> Queue.push ([`V(x,x')],i) queue) (Graph.vertices g')
           else if not (Graph.Map.totale i) then
             let e = Graph.Map.picke i in
             List.iter (fun e' -> Queue.push ([`E(e,e')],i) queue) (Graph.edges g')
           else
             return i
        | `V(x,x')::l ->
           (* Printf.printf "V: %s = %s\n" (Vertex.to_string x) (Vertex.to_string x'); *)
           assert (Graph.hasv g x);
           assert (Graph.hasv g' x');
           if Vertex.label x != Vertex.label x' then raise Exit;
           if Graph.Map.hasv i x then
             if Graph.Map.appv i x != x' then
               raise Exit
             else
               (Queue.push (l,i) queue; raise Exit);
           if injective && Graph.Map.cohasv i x' then raise Exit;
           let i = Graph.Map.addv i x x' in
           (* Map every element of the first list to an element of the second one. *)
           let rec mappings l1 l2 =
             match l1 with
             | [] -> [[]]
             | x1::l1 ->
                let m = mappings l1 l2 in
                let ans = ref [] in
                List.iter
                  (fun x2 ->
                    let m = List.map (fun l -> (x1,x2)::l) m in
                    ans := m @ !ans
                  ) l2;
                !ans
           in
           let p = mappings (Graph.vertex_pred g x) (Graph.vertex_pred g' x') in
           let s = mappings (Graph.vertex_succ g x) (Graph.vertex_succ g' x') in
           let p = List.map (List.map (fun (e,e') -> `E(e,e'))) p in
           let s = List.map (List.map (fun (e,e') -> `E(e,e'))) s in
           List.iter_pairs
             (fun p s ->
               Queue.push (p@s@l,i) queue
             ) p s
        | `E(e,e')::l ->
           assert (Graph.hase g e);
           assert (Graph.hase g' e');
           if Edge.label e != Edge.label e' then raise Exit;
           if Graph.Map.hase i e then
             if Graph.Map.appe i e != e' then
               raise Exit
             else
               (Queue.push (l,i) queue; raise Exit);
           if Graph.Map.cohase i e' then raise Exit;
           let i = Graph.Map.adde i e e' in
           let p = List.map2 (fun x x' -> `V(x,x')) (Edge.source e) (Edge.source e') in
           let s = List.map2 (fun y y' -> `V(y,y')) (Edge.target e) (Edge.target e') in
           Queue.push (p@s@l,i) queue
      with
      | Exit ->
         (* Printf.printf "fail\n"; *)
         ()
    done;
    !ans

  (** Rewriting rules. *)
  module Rule = struct
    (** A rewriting rule. *)
    type t =
      {
        label : string;
        source : term;
        target : term
      }

    (** Create a rewriting rule. *)
    let make label l r =
      assert (parallel l r);
      { label; source = l; target = r}

    (** Apply a rewriting rule. *)
    let rewrite r t =
      let l = r.source in
      let r = r.target in
      let m = matchings l t in
      if m = [] then None else
        let i = List.hd m in
        let dl = (source l)@(target l) in
        let g = graph t in
        (* Remove matched part. *)
        let g =
          let rv = List.sub (Graph.vertices (graph l)) dl in
          let re = Graph.edges (graph l) in
          let rv = List.map (Graph.Map.appv i) rv in
          let re = List.map (Graph.Map.appe i) re in
          Graph.remove g rv re
        in
        (* Add new part. *)
        let i, g =
          let dr = (source r)@(target r) in
          let i1,i2 = Graph.coprod g (graph r) in
          let g = Graph.Map.target i1 in
          (* Printf.printf "coprod:\n%s\n\n" (Graph.to_string g); *)
          let s =
            List.map2
              (fun x x' ->
                let x = Graph.Map.appv i x in
                let x = Graph.Map.appv i1 x in
                let x' = Graph.Map.appv i2 x' in
                x', x
              ) dl dr
          in
          let s x = try List.assoc x s with Not_found -> x in
          i1, Graph.Map.target (Graph.quotient g s)
        in
        let graph = g in
        let source = source t in
        let target = target t in
        let source = List.map (Graph.Map.appv i) source in
        let target = List.map (Graph.Map.appv i) target in
        Some { graph; source; target }

    let label r = r.label

    let source r = r.source

    let target r = r.target
  end

  (** Presentations. *)
  module Pres = struct
    (** A presentation. *)
    type t =
      {
        signature : Signature.t;
        rules : Rule.t list
      }

    let signature p = p.signature

    let rules p = p.rules

    (** Empty presentation. *)
    let empty = { signature = Signature.empty; rules = [] }

    (** Vertex. *)
    let getv p name =
      List.find (fun x -> Vertex.label x = name) (Signature.vertices p.signature)

    (** Edge. *)
    let gete p name =
      List.find (fun e -> Edge.label e = name) (Signature.edges p.signature)

    (** Relation. *)
    let getr p name =
      List.find (fun r -> Rule.label r = name) (rules p)

    (** Add a vertex. *)
    let addv p name =
      let v = Vertex.make name in
      let signature = Signature.addv p.signature v in
      { p with signature }

    (** Add an edge. *)
    let adde p name s t =
      let s = List.map (getv p) s in
      let t = List.map (getv p) t in
      let e = Edge.make name s t in
      let signature = Signature.adde p.signature e in
      { p with signature }

    (** Add a relation. *)
    let addr p name l r =
      let r = Rule.make name l r in
      { p with rules = r::p.rules }
  end
end
