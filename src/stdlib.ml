(** Enhanced standard library. *)

let is_found f x =
  try ignore (f x); true with Not_found -> false

(** Enumerations. *)
module Enum = struct
  (** An enumeration. *)
  type 'a t = unit -> 'a

  (** End of the enumeration. *)
  exception End

  let make f = f

  (** Empty enumeration. *)
  let empty = make (fun () -> raise End)

  let of_list l =
    let l = ref l in
    let f () =
      match !l with
      | x::l' -> l := l'; x
      | [] -> raise End
    in
    make f

  let get e = e ()

  let append e1 e2 =
    fun () ->
      try
        get e1
      with
        | End -> get e2

  let map f e =
    fun () -> f (get e)
end

(** Functions (with physical equality). *)
module Fun = struct
  let eq x y = x == y

  (** A function. *)
  type 'a t = ('a * 'a) list

  (** Nowhere defined function. *)
  let empty : 'a t = []

  (** Domain. *)
  let dom (f : 'a t) = List.map fst f

  (** Test whether an element is in the domain. *)
  let has (f:'a t) x =
    List.exists (fun (x',_) -> eq x x') f

  (** Test whether an element is in the codomain. *)
  let cohas (f:'a t) x =
    List.exists (fun (_,x') -> eq x x') f

  (** Image of an element. *)
  let app (f:'a t) x =
    List.assq x f

  (** Add a binding to a function. *)
  let add f x y : 'a t =
    assert (not (has f x));
    (x,y)::f

  (** Sequential composition of functions. *)
  let comp (f:'a t) g : 'a t =
    List.map (fun (x,y) -> x, app g y) f
end

(** Equivalence relation (wrt physical equality). *)
module Equiv = struct
  (* TODO: abstract with a functor *)
  let eq x y = x == y

  (** An equivalence relation. *)
  type 'a t = 'a Fun.t

  let empty : 'a t = Fun.empty

  (** Canonical representative. *)
  let repr (r : 'a t) x = Fun.app r x

  (** Representative for a partially defined equivalence relation. *)
  let prepr r x =
    try repr r x with Not_found -> x

  (** Test for equivalence. *)
  let equiv r x y = eq (repr r x) (repr r y)

  (** Part of the domain. *)
  let has r x =
    Fun.has r x

  (** Add to the domain. *)
  let add r x : 'a t =
    assert (not (has r x));
    Fun.add r x x

  (** Merge two elements. *)
  let merge r x y =
    assert (has r x);
    assert (has r y);
    let x = repr r x in
    let y = repr r y in
    if x == y then r else
      List.map (fun (z,z') -> z, if eq z' x then y else z') r

  (** Map a function to elements. *)
  let map f (r : 'a t) : 'b t =
    List.map (fun (x,y) -> f x, f y) r
end

(** Lists with physical equality. *)
module Listq = struct
  let hd = List.hd
  let tl = List.tl
  let map = List.map
  let map2 = List.map2
  let iter = List.iter
  let filter = List.filter
  let fold_left = List.fold_left
  let fold_left2 = List.fold_left2
  let length = List.length
  let for_all2 = List.for_all2
  let mem = List.memq
  let assoc = List.assq

  let sub l1 l2 =
    List.filter (fun x -> not (mem x l2)) l1

  (** Intersection (wrt physical equality). *)
  let inter l1 l2 =
    List.filter (fun x -> List.mem x l2) l1

  (** Remove duplicates (wrt physical equality). *)
  let rec unique = function
    | x::l when mem x l -> unique l
    | x::l -> x::(unique l)
    | [] -> []

  let iter_pairs f l1 l2 =
    iter (fun x -> iter (fun y -> f x y) l2) l1

  let pairs l1 l2 =
    let ans = ref [] in
    iter_pairs (fun x y -> ans := (x,y) :: !ans) l1 l2;
    !ans
end

module Option = struct
  let default x = function
    | Some x -> x
    | None -> x

  let get = function
    | Some x -> x
    | None -> raise Not_found
end
