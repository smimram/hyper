open Plot

module Html = Dom_html

(* let color = CSS.Color.string_of_t (CSS.Color.rgb 100 120 150) in *)
(* ctx##strokeStyle <- (Js.string color); *)
(* ctx##lineWidth <- 2.; *)
(* ctx##beginPath (); *)
(* ctx##moveTo (10.,10.); *)
(* ctx##lineTo (100.,200.); *)
(* ctx##stroke (); *)

let red = CSS.Color.string_of_t (CSS.Color.rgb 255 0 0)
let black = CSS.Color.string_of_t (CSS.Color.rgb 0 0 0)

let plot canvas vg =
  let ctx = canvas##getContext (Html._2d_) in
  let border = 10. in
  let px_of_p p =
    let width, height = canvas##width, canvas##height in
    let width = float width -. 2. *. border in
    let height = float height -. 2. *. border in
    let x = p.C.re *. width in
    let y = p.C.im *. height in
    x +. border, y +. border
  in
  ctx##clearRect (0., 0., float canvas##width, float canvas##height);
  List.iter
    (function
     | Vertex p ->
        let x,y = px_of_p p in
        (* Graphics.set_color Graphics.black; *)
     (* Graphics.fill_circle x y 5 *)
        ()
     | Edge p ->
        let x,y = px_of_p p in
        (* Graphics.set_color Graphics.red; *)
     (* Graphics.fill_circle x y 5 *)
        ()
     | Wire (p1,p2) ->
        ctx##strokeStyle <- Js.string black;
        (* ctx##lineWidth <- 2.; *)
        ctx##beginPath ();
        let x,y = px_of_p p1 in
        ctx##moveTo (x,y);
        let x,y = px_of_p p2 in
        ctx##lineTo (x,y);
        ctx##stroke ();
    ) vg;
  Graphics.synchronize ()

let plot_term canvas t =
  let w = P.make t in
  let plot () =
    plot canvas (P.plot w)
  in
  while true do
    plot ();
    P.step w 0.1
    (* Unix.sleepf 0.01 *)
  done
