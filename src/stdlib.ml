(** Enhanced standard library. *)

(** Enumerations. *)
module Enum = struct
  type 'a t = unit -> 'a

  exception End

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

(** Equivalence relation (with physical equality). *)
module Equiv = struct
  (* TODO: abstract with a functor *)
  let eq x y = x == y

  (** An equivalence relation. *)
  type 'a t = ('a * 'a) list

  let empty : 'a t = []

  (** Canonical representative. *)
  let repr (e : 'a t) x = List.assoc x e

  (** Representative for a partially defined equivalence relation. *)
  let prepr e x =
    try repr e x with Not_found -> x

  (** Test for equivalence. *)
  let equiv e x y = eq (repr e x) (repr e y)

  (** Part of the domain. *)
  let has e x =
    try ignore (repr e x); true with Not_found -> false

  (** Add to the domain. *)
  let add e x : 'a t =
    assert (not (has e x));
    (x,x)::e

  (** Merge two elements. *)
  let merge e x y =
    assert (has e x);
    assert (has e y);
    let x = repr e x in
    let y = repr e y in
    List.map (fun (z,z') -> z, if eq z' x then y else z') e
end

(** Functions (with physical equality). *)
module Fun = struct
  let eq x y = x == y

  (** A function. *)
  type 'a t = ('a * 'a) list

  (** Nowhere defined function. *)
  let empty = []

  (** Test whether an element is in the domain. *)
  let has (f:'a t) x =
    List.exists (fun (x',_) -> eq x x') f

  (** Image of an element. *)
  let get (f:'a t) x =
    List.assq x f

  (** Add a binding to a function. *)
  let add f x y : 'a t =
    assert (not (has f x));
    (x,y)::f
end

module List = struct
  include List

  let interq l1 l2 =
    List.filter (fun x -> List.memq x l2) l1
end
