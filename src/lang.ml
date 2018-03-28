(*pp camlp4o *)

(** Language. *)

open Stdlib
open Common

module T = Graph.Term
module P = Graph.Term.Pres

let pres = ref P.empty

(* Special vertex for PROs *)
let pro_kind = "|"
let () = pres := P.addv !pres pro_kind
let kinds_of_int n = List.init n (fun _ -> pro_kind)

let expr =
  let lexer = Genlex.make_lexer ["*";"+";"(";")"] in
  fun s ->
  let rec comp = parser
      | [< t1 = atom; 'Genlex.Kwd "*"; t2 = comp >] -> T.comp t1 t2
      | [< t = tens >] -> t
  and tens = parser
      | [< t1 = atom; 'Genlex.Kwd "+"; t2 = comp >] -> T.tens t1 t2
      | [< t = atom >] -> t
  and atom = parser
      | [< 'Genlex.Int n >] -> T.id (List.map (P.getv !pres) (kinds_of_int n))
      | [< 'Genlex.Kwd "("; t = comp; 'Genlex.Kwd ")" >] -> t
  in
  atom (lexer (Stream.of_string s))

(** Execute a command. *)
let command cmd  =
  let cmd = String.split_on_char ' ' cmd in
  let cmd = List.filter (fun s -> s <> "") cmd in
  match cmd with
  | ["kind";k] -> pres := P.addv !pres k
  | ["op";o;":";s;"->";t] when String.is_int s && String.is_int t ->
     let s = kinds_of_int (int_of_string s) in
     let t = kinds_of_int (int_of_string t) in
     pres := P.adde !pres o s t
  | ["rule";r;":";s;"=>";t] ->
     let s = expr s in
     let t = expr t in
     pres := P.addr !pres r s t
  | ["show";e] ->
     let t = expr e in
     Printf.printf "%s\n%!" (T.to_string t)
  | cmd::_ -> error ("Unknown command: " ^ cmd)
  | [] -> ()
