(** Graph plotting library. *)

open Stdlib

open Graph

module Complex = struct
  include Complex

  let cmul a c =
    { re = a *. c.re; im = a *. c.im }

  let normalize c =
    cmul (1. /. norm c) c

  let map f c =
    { re = f c.re; im = f c.im }
end
module C = Complex

module Physics = struct
  (** Maximum speed. *)
  let max_speed = ref 1000000.
  (** Damping factor. *)
  let damping = ref 0.95
  (** Charge of a particle. *)
  let charge = ref 0.1
  (** Length of a spring. *)
  let spring_length = ref 0.2
  (** Stiffness of a spring. *)
  let spring_k = ref 0.5

  (** An element of a graph. *)
  type element =
    | Vertex of Term.vertex (** a vertex *)
    | Edge of Term.edge (** an edge *)

  (** A point. *)
  type point =
    {
      element : element; (** represented element *)
      m : float; (** mass *)
      fixed : bool; (** can it move? *)
      mutable p : C.t; (** position *)
      mutable v : C.t; (** speed *)
      mutable a : C.t  (** acceleration *)
    }

  (** Points. *)
  module Point = struct
    type t = point

    let make ?(fixed=false) ?x ?y e : t =
      let p =
        match x,y with
        | Some x, Some y -> { C.re = x; im = y }
        | _ -> { re = Random.float 1.; im = Random.float 1. }
      in
      {
        element = e;
        m = 1.;
        fixed;
        p;
        v = { re = Random.float 2. -. 1.; im = Random.float 2. -. 1. };
        a = { re = Random.float 2. -. 1.; im = Random.float 2. -. 1. }
      }

    let vertex ?fixed ?x ?y v = make ?fixed ?x ?y (Vertex v)

    let edge e = make (Edge e)
  end

  (** A spring. *)
  type spring =
    {
      source : point;
      target : point
    }

  (** Springs. *)
  module Spring = struct
    type t = spring

    let make source target : t = { source; target }

    let source (s:t) = s.source

    let target (s:t) = s.target
  end

  (** A physical world. *)
  type world =
    {
      mutable points : point list;
      mutable springs : spring list;
    }

  (** Empty world. *)
  let empty = { points = []; springs = [] }

  (** Add a point. *)
  let add_point w p = w.points <- p::w.points

  (** Add a spring. *)
  let add_spring w s = w.springs <- s::w.springs

  (** Point associated to given vertex. *)
  let vertex w v =
    List.find
      (fun p ->
        match p.element with
        | Vertex v' -> v' == v
        | _ -> false
      ) w.points

  (** Is a given vertex defined? *)
  let has_vertex w v =
    is_found (vertex w) v

  (** Action of a force on a point. *)
  let act f p =
    p.a <- C.add p.a (C.cmul (1. /. p.m) f)

  (** Apply Coulomb law. *)
  let coulomb w =
    let rec iter f = function
      | x::l -> List.iter (f x) l; iter f l
      | [] -> ()
    in
    iter
      (fun p1 p2 ->
        let r = C.sub p2.p p1.p in
        let d = C.norm r +. 0.01 in
        let q = !charge in
        let f = C.cmul (q *. q /. d) r in
        act f p2;
        act (C.neg f) p1
      ) w.points

  (** Apply Hooke's law. *)
  let hooke w =
    List.iter
      (fun s ->
        let p1 = s.source in
        let p2 = s.target in
        let r = C.sub p2.p p1.p in
        let ds = !spring_length -. C.norm r in
        let f = C.cmul (!spring_k *. ds /. C.norm r) r in
        act f p2;
        act (C.neg f) p1
      ) w.springs

  (** Repulse from boundary. *)
  let boundary w =
    List.iter
      (fun p ->
        let clip x =
          let c = 0.001 in
          min (1. -. c) (max c x)
        in
        let c = 1000. in
        let x = clip p.p.re in
        let y = clip p.p.im in
        let fx = 1. /. (c *. x) +. 1. /. (c *. (x -. 1.)) in
        let fy = 1. /. (c *. y) +. 1. /. (c *. (y -. 1.)) in
        let f = { C.re = fx ; im = fy } in
        act f p
      ) w.points

  let clear_a w =
    List.iter (fun p -> p.a <- C.zero) w.points

  (** Update velocity. *)
  let update_v w dt =
    List.iter
      (fun p ->
        p.v <- C.add p.v (C.cmul dt p.a);
        p.v <- C.cmul !damping p.v;
        let vv = C.norm p.v in
        if vv > !max_speed then p.v <- C.cmul (!max_speed /. vv) p.v
      ) w.points

  (** Update position. *)
  let update_p w dt =
    let clip p =
      let clipped = ref false in
      let clip x = if x < 0. then (clipped := true; 0.) else if x > 1. then (clipped := true; 1.) else x in
      p.p <- C.map clip p.p;
      (* if !clipped then p.v <- C.zero; *)
      (* if !clipped then p.a <- C.zero; *)
    in
    List.iter
      (fun p ->
        if not p.fixed then p.p <- C.add p.p (C.cmul dt p.v);
        clip p
      ) w.points

  (** Total energy. *)
  let energy w =
    List.fold_left
      (fun e p ->
        let v = C.norm p.v in
        e +. p.m *. v *. v /. 2.
      ) 0. w.points

  (** Perform a simulation step. *)
  let step w dt =
    clear_a w;
    coulomb w;
    boundary w;
    hooke w;
    update_v w dt;
    update_p w dt
end
module P = Physics

let make t =
  let g = Term.graph t in
  let w = P.empty in
  (* Source. *)
  let () =
    let source = Term.source t in
    let n = List.length source in
    if n = 1 then
      let v = List.hd source in
      let p = P.Point.vertex ~fixed:true ~x:0. ~y:0.5 v in
      P.add_point w p
    else
    List.iteri
      (fun i v ->
        let y = float i /. (float (n-1)) in
        let p = P.Point.vertex ~fixed:true ~x:0. ~y v in
        P.add_point w p
      ) source
  in
  (* Target. *)
  let () =
    let target = Term.target t in
    let n = List.length target in
    if n = 1 then
      let v = List.hd target in
      let p = P.Point.vertex ~fixed:true ~x:1. ~y:0.5 v in
      P.add_point w p
    else
      List.iteri
        (fun i v ->
          let y = float i /. (float (n-1)) in
          let p = P.Point.vertex ~fixed:true ~x:1. ~y v in
          P.add_point w p
        ) target
  in
  (* Vertices. *)
  let () =
    List.iter
      (fun v ->
        let p = P.Point.vertex v in
        P.add_point w p
      ) (Listq.sub (Graph.vertices g) ((Term.source t)@(Term.target t)))
  in 
  (* Edges. *)
  let () =
    List.iter
      (fun e ->
        let pe = P.Point.edge e in
        P.add_point w pe;
        let ls = List.map (fun v -> P.vertex w v, pe) (Edge.source e) in
        let lt = List.map (fun v -> pe, P.vertex w v) (Edge.target e) in
        List.iter
          (fun (p1,p2) ->
            let s = P.Spring.make p1 p2 in
            P.add_spring w s
          ) (ls@lt)
      ) (Graph.edges g)
  in
  w

let graphics t =
  Graphics.open_graph "";
  let w = make t in
  let border = 10 in
  let plot () =
    let px_of_p p =
      let width = Graphics.size_x () - 2 * border in
      let height = Graphics.size_y () - 2 * border in
      let x = p.C.re *. float width in
      let y = p.C.im *. float height in
      int_of_float x + border, int_of_float y + border
    in
    List.iter
      (fun p ->
        let color =
          match p.P.element with
          | Vertex _ -> Graphics.black
          | Edge _ -> Graphics.red
        in
        let x, y = px_of_p p.P.p in
        Graphics.set_color color;
        Graphics.fill_circle x y 5
      ) w.points;
    Graphics.set_color Graphics.black;
    List.iter
      (fun s ->
        let x,y = px_of_p (P.Spring.source s).P.p in
        Graphics.moveto x y;
        let x,y = px_of_p (P.Spring.target s).P.p in
        Graphics.lineto x y
      ) w.springs
  in
  plot ();
  while not (Graphics.key_pressed ()) do
    P.step w 0.1;
    Graphics.clear_graph ();
    plot ();
    Unix.sleepf 0.01
  done
