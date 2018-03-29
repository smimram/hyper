(** Common functions. *)

let print_string_fun =
  ref (fun s -> print_string s; flush stdout)

(** Printing function. *)
let print s = !print_string_fun s

let printf f = Printf.ksprintf print f

(** Error. *)
let error e =
  print ("EE: " ^ e ^ "\n")
