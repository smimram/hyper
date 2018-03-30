(** Interaction with webpage. *)

module Html = Dom_html

let doc = Html.document
let button txt action =
  let button_type = Js.string "button" in
  let b = Html.createInput ~_type:button_type doc in
  b##.value := Js.string txt;
  b##.onclick := Dom_html.handler (fun _ -> action (); Js._true);
  b

let debug s =
  Firebug.console##debug (Js.string s)

let loop s =
  Lang.command s

let run _ =
  let top =
    Js.Opt.get
      (doc##getElementById(Js.string "toplevel"))
      (fun () -> assert false)
  in

  (* let output = Html.createDiv doc in *)
  (* output##id := Js.string "output"; *)
  (* output##style##whiteSpace := Js.string "pre"; *)
  (* Dom.appendChild top output; *)

  (* Canvas. *)
  let canvas = Html.createCanvas doc in
  canvas##.id := Js.string "graph";
  canvas##.width := 600;
  canvas##.height := 300;
  Dom.appendChild top canvas;
  Lang.plot_term := (WebPlot.plot_term canvas);

  (* Text box. *)
  let textbox = Html.createTextarea doc in
  textbox##.id := Js.string "input";
  textbox##.cols := 80;
  textbox##.rows := 25;
  (* textbox##value := Js.string "# "; *)
  Dom.appendChild top textbox;
  Dom.appendChild top (Html.createBr doc);
  textbox##focus;
  textbox##select;

  (* Current offset in textbox. *)
  let tb_off = ref 0 in
  let print s =
    let s = Js.to_string textbox##.value ^ s in
    tb_off := String.length s;
    textbox##.value := Js.string s;
    (* Scroll down. *)
    Js.Unsafe.set textbox (Js.string "scrollTop") (Js.Unsafe.get textbox (Js.string "scrollHeight"))
  in
  let read () =
    let s = Js.to_string textbox##.value in
    let cmd = String.sub s !tb_off (String.length s - !tb_off) in
    tb_off := String.length s;
    cmd
  in

  Common.print_string_fun := print;
  print "# ";

  let b =
    button
      "Send"
      (fun () ->
        let s = read () in
        let s =
          let s = ref s in
          let remove_last () =
            if !s = "" then false else
              let c = !s.[String.length !s - 1] in
              c = '\n' || c = '\r'
          in
          while remove_last () do
            (* remove trailing \n *)
            s := String.sub !s 0 (String.length !s - 1)
          done;
          !s
        in
        loop s;
        textbox##focus;
        doc##.documentElement##.scrollTop := doc##.body##.scrollHeight;
        print "# ")
  in
  b##.id := Js.string "send";
  Dom.appendChild top b;

  ignore (Js.Unsafe.eval_string "init();");

  loop "help";
  loop "op m : 2 -> 1";
  loop "ops";
  loop "plot (m*1);m";

  Js._false

let () =
  Random.self_init ();
  Html.window##.onload := Html.handler run
