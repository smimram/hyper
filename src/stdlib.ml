(** Enhanced standard library. *)

let is_found f x =
  try ignore (f x); true with Not_found -> false

module List = struct
  include List

  let may_map f l =
    let rec aux = function
      | x::l ->
         (
           match f x with
           | Some x -> x :: (aux l)
           | None -> aux l
         )
      | [] -> []
    in
    aux l
  
  (** Iterate over pairs of elements of two lists. *)
  let iter_pairs f l1 l2 =
    iter (fun x -> iter (fun y -> f x y) l2) l1
end

module Char = struct
  include Char

  let is_digit c =
    let n = code c in
    code '0' <= n && n <= code '9'
end

module String = struct
  include String

  let is_int s =
    try
      for i = 0 to String.length s - 1 do
        if not (Char.is_digit s.[i]) then raise Exit
      done;
      true
    with
    | Exit -> false
end

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
  let iteri = List.iteri
  let filter = List.filter
  let fold_left = List.fold_left
  let fold_left2 = List.fold_left2
  let length = List.length
  let for_all2 = List.for_all2
  let mem = List.memq
  let assoc = List.assq
  let find = List.find

  (** Subtract a list from another list. *)
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

  let iter_pairs = List.iter_pairs

  (** Pairs of elements of two lists. *)
  let pairs l1 l2 =
    let ans = ref [] in
    iter_pairs (fun x y -> ans := (x,y) :: !ans) l1 l2;
    !ans

  let for_all_pairs f l1 l2 =
    try
      iter_pairs (fun x y -> if not (f x y) then raise Exit) l1 l2;
      true
    with
    | Exit -> false
end

(** Option types. *)
module Option = struct
  let default x = function
    | Some x -> x
    | None -> x

  let get = function
    | Some x -> x
    | None -> raise Not_found
end

(** Arrays with physical equality. *)
module Arrayq = struct
  let index a x =
    let ans = ref (-1) in
    try
      for i = 0 to Array.length a - 1 do
        if a.(i) == x then
          (
            ans := x;
            raise Exit
          )
      done;
      raise Not_found
    with
    | Exit -> !ans
end

(** Posets with physical equality. *)
module Poset = struct
  (** A poset. *)
  (* This is a list of pairs of elements, improve that. *)
  type 'a t = ('a * 'a) list

  (** Create a poset on given elements. *)
  let create l : 'a t =
    List.map (fun x -> x,x) l

  (** Test for inequality. *)
  let leq (p:'a t) x y =
    List.exists (fun (x',y') -> x == x' && y == y') p

  let lt p x y =
    x != y && leq p x y

  (** Add a dependency between elements. *)
  let add p x y =
    let p = ref p in
    let queue = Queue.create () in
    Queue.add (x,y) queue;
    while not (Queue.is_empty queue) do
      let x,y = Queue.pop queue in
      if not (leq !p x y) then
        let l = List.may_map (fun (w,x') -> if x' == x then Some (x,y) else None) !p in
        let r = List.may_map (fun (y',z) -> if y' == y then Some (x,z) else None) !p in
        p := (x,y) :: !p;
        List.iter (fun r -> Queue.add r queue) l;
        List.iter (fun r -> Queue.add r queue) r
    done;
    !p
end
