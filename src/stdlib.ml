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

(** Functions (with physical equality). *)
module Fun = struct
  let eq x y = x == y

  (** A function. *)
  type 'a t = ('a * 'a) list

  (** Nowhere defined function. *)
  let empty : 'a t = []

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

(** Equivalence relation (wrt physical equality). *)
module Equiv = struct
  (* TODO: abstract with a functor *)
  let eq x y = x == y

  (** An equivalence relation. *)
  type 'a t = 'a Fun.t

  let empty : 'a t = Fun.empty

  (** Canonical representative. *)
  let repr (e : 'a t) x = Fun.get e x

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
    Fun.add e x x

  (** Merge two elements. *)
  let merge e x y =
    assert (has e x);
    assert (has e y);
    let x = repr e x in
    let y = repr e y in
    List.map (fun (z,z') -> z, if eq z' x then y else z') e
end


module List = struct
  include List

  (** Remove duplicates (wrt physical equality). *)
  let rec uniq = function
    | x::l when memq x l -> uniq l
    | x::l -> x::(uniq l)
    | [] -> []

  (** Intersection (wrt physical equality). *)
  let interq l1 l2 =
    List.filter (fun x -> List.memq x l2) l1
end
