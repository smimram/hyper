open Stdlib
open Common
open Plot

module Html = Dom_html

let plot canvas vg =
  let ctx = canvas##getContext (Html._2d_) in
  let border = 10. in
  let px_of_p p =
    let width, height = canvas##.width, canvas##.height in
    let width = float width -. 2. *. border in
    let height = float height -. 2. *. border in
    let x = p.C.re *. width in
    let y = p.C.im *. height in
    x +. border, y +. border
  in
  ctx##clearRect 0. 0. (float canvas##.width) (float canvas##.height);
  List.iter
    (function
     | Vertex p ->
        let x,y = px_of_p p in
        let r = 5. in
        ctx##beginPath;
        ctx##.fillStyle := Js.string "#000000";
        ctx##arc x y r 0. (2. *. 3.1416) (Js.bool false);
        ctx##fill;
        ctx##stroke
     | Edge p ->
        let x,y = px_of_p p in
        (* Graphics.set_color Graphics.red; *)
        (* Graphics.fill_circle x y 5 *)
                let x,y = px_of_p p in
        let r = 5. in
        ctx##beginPath;
        (* ctx##.strokeStyle := Js.string "#ff0000"; *)
        ctx##.fillStyle := Js.string "#ff0000";
        ctx##arc x y r 0. (2. *. 3.1416) (Js.bool false);
        ctx##fill;
        ctx##stroke
     | Wire (p1,p2) ->
        ctx##.strokeStyle := Js.string "#000000";
        ctx##beginPath;
        let x,y = px_of_p p1 in
        ctx##moveTo x y;
        let x,y = px_of_p p2 in
        ctx##lineTo x y;
        ctx##stroke;
    ) vg

let (>>=) = Lwt.bind

let plot_term canvas t =
  let w = P.make t in
  let plot () = plot canvas (P.plot w) in
  let rec aux () =
    Lwt_js.sleep 0.2 >>= fun () ->
    if P.energy w >= !P.min_energy then
      (
        plot ();
        P.step w 0.1
      );
    aux ()
  in
  ignore (aux ())

let plot_terms canvas t =
  let w = ref (P.empty ()) in
  let plot () = plot canvas (P.plot !w) in
  let rec aux () =
    Lwt_js.sleep 0.05 >>= fun () ->
    try
    if P.energy !w >= !P.min_energy then
      (
        plot ();
        P.step !w 0.1
      )
    else
      w := P.update !w (Enum.get t);
    aux ()
    with
    | Enum.End -> aux ()
  in
  ignore (aux ())
