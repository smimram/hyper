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
  (* Printf.printf "expr: '%s'\n%!" s; *)
  let rec comp = parser
      | [< t = tens; f = comps >] -> f t
  and comps = parser
      | [< 'Genlex.Kwd "*"; t' = tens; f = comps >] -> fun t -> f (T.comp t t')
      | [< >] -> fun t -> t
  and tens = parser
      | [< t = atom; f = tenss >] -> f t
  and tenss = parser
      | [< 'Genlex.Kwd "+"; t' = atom; f = tenss >] -> fun t -> f (T.tens t t')
      | [< >] -> fun t -> t
  and atom = parser
      | [< 'Genlex.Int n >] -> T.id (List.map (P.getv !pres) (kinds_of_int n))
      | [< 'Genlex.Ident s >] -> T.generator (P.gete !pres s)
      | [< 'Genlex.Kwd "("; t = comp; 'Genlex.Kwd ")" >] -> t
  in
  comp (lexer (Stream.of_string s))

(** Execute a command. *)
let command cmd  =
  (* printf "cmd: '%s'\n" cmd; *)
  let cmd = String.split_on_char ' ' cmd in
  let cmd = List.filter (fun s -> s <> "") cmd in
  match cmd with
  | ["kind";k] -> pres := P.addv !pres k
  | ["op";o;":";s;"->";t]
  | ["op";o;s;t] when String.is_int s && String.is_int t ->
     let s = kinds_of_int (int_of_string s) in
     let t = kinds_of_int (int_of_string t) in
     pres := P.adde !pres o s t
  | ["rule";r;":";s;"=>";t]
  | ["rule";r;s;t] ->
     let s = expr s in
     let t = expr t in
     pres := P.addr !pres r s t
  | ["show";e] ->
     let t = expr e in
     print (T.to_string t ^ "\n")
  | ["normalize";e] ->
     let t = ref (expr e) in
     print (T.to_string !t ^ "\n\n");
     let loop = ref true in
     while !loop do
       try
         List.iter
           (fun r ->
             match T.Rule.rewrite r !t with
             | Some t' ->
                t := t';
                print (T.to_string !t ^ "\n\n");
                raise Exit
             | None -> ()
           ) (P.rules !pres);
         loop := false
       with
       | Exit -> ()
     done
  | ["ops"] -> print (String.concat " " (List.map Graph.Edge.label (Graph.Signature.edges (P.signature !pres))) ^ "\n")
  | ["rules"] -> print (String.concat " " (List.map T.Rule.label (P.rules !pres)) ^ "\n")
  | ["help"] -> print ("You're on your own, sorry.\n")
  | cmd::_ -> error ("Unknown command: " ^ cmd)
  | [] -> ()

let command cmd =
  List.iter command (String.split_on_char '\n' cmd)
