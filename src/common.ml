(** Common functions. *)

let print_string_fun =
  ref print_string

(** Printing function. *)
let print s = !print_string_fun s

let printf f = Printf.ksprintf print f

(** Error. *)
let error e =
  print ("EE: " ^ e ^ "\n")
