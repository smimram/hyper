(** Graph plotting library. *)

(* TODO: remove torsion springs which are not used in the end *)

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

  let to_string c =
    Printf.sprintf "%f+i%f" c.re c.im
end
module C = Complex

(** Vector graphics. *)
type vg =
  | Vertex of C.t
  | Edge of C.t
  | Wire of C.t * C.t

module Physics = struct
  (** Maximum speed. *)
  let max_speed = ref 1.
  (** Damping factor. *)
  let damping = ref 0.95
  (** Charge of a particle. *)
  let charge = ref 0.05
  (** Length of a spring. *)
  let spring_length = ref 0.1
  (** Stiffness of a spring. *)
  let spring_k = ref 1.
  (** Angle of a torsion spring. *)
  let tspring_angle = ref (3.1416 /. 4.)
  (** Stiffness of a torsion spring. *)
  let tspring_k = ref 0.1
  (** Minimal energy in order to proceed with simulation. *)
  let min_energy = ref 0.001

  (** An element of a graph. *)
  type element =
    [ `Vertex of Term.vertex (** a vertex *)
    | `Edge of Term.edge (** an edge *)
    ]

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

    let make ?(fixed=false) ?p e : t =
      (* let p = Option.default { C.re = Random.float 1.; im = Random.float 1. } p in *)
      let p = Option.default { C.re = 0.5; im = Random.float 0.5 } p in
      let r () = Random.float 0.2 -. 0.1 in
      {
        element = e;
        m = 1.;
        fixed;
        p;
        v = { re = r (); im = r () };
        a = { re = r (); im = r () }
      }

    let vertex ?fixed ?p v = make ?fixed ?p (`Vertex v)

    let edge ?p e = make ?p (`Edge e)

    let copy p =
      { p with m = p.m}
  end

  (** A spring. *)
  type spring =
    {
      source : point;
      target : point;
      d : C.t (* direction of the spring *)
    }

  (** Springs. *)
  module Spring = struct
    type t = spring

    let make ?(d=(C.cmul !spring_length C.one)) source target : t = { source; target; d }

    let source (s:t) = s.source

    let target (s:t) = s.target

    let copy s = { s with source = s.source }
  end

  (** Torsion springs. *)
  module TSpring = struct
    (** A torsion spring tends to keep the angle s-m-t constant. *)
    type t =
      {
        source : point;
        middle : point;
        target : point
      }

    let make source middle target = { source; middle; target }
  end

  (** A physical world. *)
  type world =
    {
      mutable points : point list;
      mutable springs : spring list;
      mutable tsprings : TSpring.t list;
    }

  (** Empty world. *)
  let empty () = { points = []; springs = []; tsprings = [] }

  (** Add a point. *)
  let add_point w p = w.points <- p::w.points

  (** Add a spring. *)
  let add_spring w s = w.springs <- s::w.springs

  let add_tspring w s = w.tsprings <- s::w.tsprings

  (** Point associated to given vertex. *)
  let vertex w v =
    List.find
      (fun p ->
        match p.element with
        | `Vertex v' -> v' == v
        | _ -> false
      ) w.points

  (** Is a given vertex defined? *)
  let has_vertex w v =
    is_found (vertex w) v

  (** Point associated to given edge. *)
  let edge w e =
    List.find
      (fun p ->
        match p.element with
        | `Edge e' -> e' == e
        | _ -> false
      ) w.points

  let has_edge w e =
    is_found (edge w) e

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
        let d = max (C.norm r) 0.01 in
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

  (** Directed springs. *)
  let dspring w =
    List.iter
      (fun s ->
        (* Default direction for spring. *)
        (* let r0 = C.cmul !spring_length C.one in *)
        let r0 = s.d in
        let p1 = s.source in
        let p2 = s.target in
        let r = C.sub p2.p p1.p in
        let f = C.cmul !spring_k (C.sub r0 r) in
        act f p2;
        act (C.neg f) p1
      ) w.springs

  (** Directed angular springs. *)
  let tspring w =
    List.iter
      (fun s ->
        let l1 = C.sub s.TSpring.source.p s.middle.p in
        let l2 = C.sub s.target.p s.middle.p in
        let l1 = C.normalize l1 in
        let l2 = C.normalize l2 in
        let a = C.div l2 l1 in
        let a0 = C.polar 1. !tspring_angle in
        let f = C.cmul !tspring_k (C.sub a0 a) in
        act (C.mul l1 f) s.target;
        act (C.mul l2 (C.neg f)) s.source
      ) w.tsprings

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
        if vv > !max_speed then p.v <- C.cmul (!max_speed /. vv) p.v;
        if p.fixed then p.v <- C.zero
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
    (* boundary w; *)
    (* hooke w; *)
    dspring w;
    (* tspring w; *)
    update_v w dt;
    update_p w dt

  (** Plot. *)
  let plot w =
    let ans = ref [] in
    let add x = ans := x :: !ans in
    (* Points *)
    List.iter
      (fun p ->
        match p.element with
        | `Vertex _ -> add (Vertex p.p)
        | `Edge _ -> add (Edge p.p)
      ) w.points;
    (* Lines *)
    List.iter
      (fun s ->
        add (Wire ((Spring.source s).p, (Spring.target s).p))
      ) w.springs;
    !ans

  (** Update the physical model of a term. *)
  (* TODO: use a morphism instead of the identity *)
  let update w0 t =
    let g = Term.graph t in
    let w = empty () in
    (* Source. *)
    let () =
      let source = Term.source t in
      let n = List.length source in
      if n = 1 then
        let v = List.hd source in
        let p = Point.vertex ~fixed:true ~p:{C.re=0.; im=0.5} v in
        add_point w p
      else
        List.iteri
          (fun i v ->
            let y = float i /. (float (n-1)) in
            let p = Point.vertex ~fixed:true ~p:{C.re=0.; im=y} v in
            add_point w p
          ) source
    in
    (* Target. *)
    let () =
      let target = Term.target t in
      let n = List.length target in
      if n = 1 then
        let v = List.hd target in
        let p = Point.vertex ~fixed:true ~p:{C.re=1.; im=0.5} v in
        add_point w p
      else
        List.iteri
          (fun i v ->
            let y = float i /. (float (n-1)) in
            let p = Point.vertex ~fixed:true ~p:{C.re=1.; im=y} v in
            add_point w p
          ) target
    in
    (* Vertices. *)
    let () =
      List.iter
        (fun v ->
          (* if has_vertex w0 v then Printf.printf "reuse vertex: %s\n%!" (Vertex.to_string v); *)
          (* Try to reuse previous position. *)
          let v =
            try
              Point.copy (vertex w0 v)
            with
            | Not_found -> Point.vertex v
          in
          add_point w v
        ) (Listq.sub (Graph.vertices g) ((Term.source t)@(Term.target t)))
    in 
    (* Edges. *)
    let () =
      List.iter
        (fun e ->
          (* Point *)
          (* if has_edge w0 e then Printf.printf "reuse edge: %s\n%!" (Edge.to_string e); *)
          let p = try Some (edge w0 e).p with Not_found -> None in
          let pe = Point.edge ?p e in
          add_point w pe;
          (* Springs *)
          (*
          let ls = List.map (fun v -> vertex w v, pe) (Edge.source e) in
          let lt = List.map (fun v -> pe, vertex w v) (Edge.target e) in
          List.iter
            (fun (p1,p2) ->
              let s = Spring.make p1 p2 in
              add_spring w s
            ) (ls@lt);
           *)
          let n = float (List.length (Edge.source e) - 1) in
          List.iteri
            (fun i v ->
              let d = C.cmul !spring_length C.one in
              let i = float i -. n /. 2. in
              let d = C.add d { re = 0.; im = !spring_length *. i } in
              let v = vertex w v in
              add_spring w (Spring.make ~d v pe)
            ) (Edge.source e);
          let n = float (List.length (Edge.target e) - 1) in
          List.iteri
            (fun i v ->
              let d = C.cmul !spring_length C.one in
              let i = float i -. n /. 2. in
              let d = C.add d { re = 0.; im = !spring_length *. (-. i) } in
              let v = vertex w v in
              add_spring w (Spring.make ~d pe v)
            ) (Edge.target e);
          (* Torsion springs *)
          let rec iter f = function
            | x::y::l -> f x y; iter f (y::l)
            | _ -> ()
          in
          iter (fun x y -> add_tspring w (TSpring.make (vertex w x) pe (vertex w y))) (Edge.source e);
          iter (fun y x -> add_tspring w (TSpring.make (vertex w x) pe (vertex w y))) (Edge.target e)
        ) (Graph.edges g)
    in
    w

  (** Build the physical model of a term. *)
  let make t =
    update (empty ()) t
end
module P = Physics

let graphics_init () =
  Graphics.open_graph "";
  Graphics.auto_synchronize true

let graphics_plot vg =
  let border = 10 in
  let px_of_p p =
    let width, height = Graphics.size_x (), Graphics.size_y () in
    let width = width - 2 * border in
    let height = height - 2 * border in
    let x = p.C.re *. float width in
    let y = p.C.im *. float height in
    int_of_float x + border, int_of_float y + border
  in
  Graphics.clear_graph ();
  List.iter
    (function
     | Vertex p ->
        let x,y = px_of_p p in
        Graphics.set_color Graphics.black;
        Graphics.fill_circle x y 5
     | Edge p ->
        let x,y = px_of_p p in
        Graphics.set_color Graphics.red;
        Graphics.fill_circle x y 5
     | Wire (p1,p2) ->
        Graphics.set_color Graphics.black;
        let x,y = px_of_p p1 in
        Graphics.moveto x y;
        let x,y = px_of_p p2 in
        Graphics.lineto x y
    ) vg;
  Graphics.synchronize ()

let graphics_term t =
  graphics_init ();
  let w = P.make t in
  let plot () =
    graphics_plot (P.plot w)
  in
  while not (Graphics.key_pressed ()) (* && P.energy w >= 0.0001 *) do
    plot ();
    P.step w 0.1;
    Unix.sleepf 0.01
  done

let graphics_terms t =
  graphics_init ();
  let w = ref (P.empty ()) in
  let plot () = graphics_plot (P.plot !w) in
  try
    while true do
      let t = Enum.get t in
      Printf.printf "graphics:\n%s\n%!" (Term.to_string t);
      w := P.update !w t;
      (* plot (); Unix.sleep 1; *)
      while not (Graphics.key_pressed ()) && P.energy !w >= !P.min_energy do
        plot ();
        P.step !w 0.1;
        Unix.sleepf 0.01
      done;
      if Graphics.key_pressed () then ignore (Graphics.read_key ());
      (* Unix.sleepf 1. *)
    done
  with
  | Enum.End ->
     while not (Graphics.key_pressed ()) do
       plot ();
       P.step !w 0.1;
       Unix.sleepf 0.01
     done
