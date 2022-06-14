open StructuredType

module type S =
sig
  include PartiallyOrderedType
  val id : t
  val is_id : t -> bool
  val compose : t -> t -> t
end

module Int :
sig
  include S
  val of_int : int -> t
  val to_int : t -> int
end
=
struct
  type t = int
  let of_int x : t = x
  let to_int x : int = x
  let id = 0
  let equal = Int.equal
  let is_id = function 0 -> true | _ -> false
  let lt : int -> int -> bool = (<)
  let leq : int -> int -> bool = (<=)
  let compose : int -> int -> int = (+)
  let dump = Format.pp_print_int
end

module WithConst (Base : S) :
sig
  include S
  val act : Base.t -> t
  val const : Base.t -> t
end
=
struct
  type t = Act of Base.t | Const of Base.t
  let act x = Act x
  let const x = Const x
  let id = act Base.id
  let equal x y =
    match x, y with
    | Act x, Act y -> Base.equal x y
    | Const x, Const y -> Base.equal x y
    | _ -> false
  let is_id = function Act s -> Base.is_id s | _ -> false
  let lt x y =
    match x, y with
    | Act x, Act y -> Base.lt x y
    | Const x, Const y -> Base.lt x y
    | _ -> false
  let leq x y =
    match x, y with
    | Act x, Act y -> Base.leq x y
    | Const x, Const y -> Base.leq x y
    | _ -> false
  let compose x y =
    match x, y with
    | _, Const _ -> y
    | Const x, Act y -> const (Base.compose x y)
    | Act x, Act y -> act (Base.compose x y)
  let dump fmt =
    function
    | Const x ->
      Format.fprintf fmt "@[<1>(const@ @[%a@])@]" Base.dump x
    | Act x ->
      Format.fprintf fmt "@[<1>(act@ @[%a@])@]" Base.dump x
end

module Fractal (Base : S) :
sig
  include S
  val embed : Base.t -> t
  val push : Base.t -> t -> t
end
=
struct
  type t = Base.t * Base.t list

  let embed s : t = s, []
  let push s1 (s2, s2s) = s1, (s2 :: s2s)

  let id = embed Base.id

  let is_id = function s, [] -> Base.is_id s | _ -> false

  let equal (i1, is1) (i2, is2) =
    List.equal Base.equal (i1 :: is1) (i2 :: is2)

  let rec lt xs ys =
    match xs, ys with
    | [], [] -> false
    | [], _ -> true
    | _::_, [] -> false
    | x::xs, y::ys -> Base.lt x y || (Base.equal x y && lt xs ys)

  let lt (i1, is1) (i2, is2) = lt (i1 :: is1) (i2 :: is2)

  let rec leq xs ys =
    match xs, ys with
    | [], _ -> true
    | _::_, [] -> false
    | x::xs, y::ys -> Base.lt x y || (Base.equal x y && leq xs ys)

  let leq (i1, is1) (i2, is2) = leq (i1 :: is1) (i2 :: is2)

  let rec compose s1 s2 =
    match s1, s2 with
    | (s1, []), (s2, s2s) -> Base.compose s1 s2, s2s
    | (s1, (s11 :: s1s)), _ -> push s1 (compose (s11, s1s) s2)

  let dump fmt (s, ss) =
    if ss = [] then
      Base.dump fmt s
    else
      Format.fprintf fmt "@[<1>(%a)@]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ")@,.(") Base.dump)
        (s :: ss)
end

module LexicalPair (X : S) (Y : S) :
sig
  include S
  val pair : X.t -> Y.t -> t
  val fst : t -> X.t
  val snd : t -> Y.t
  val inl : X.t -> t
  val inr : Y.t -> t
end
=
struct
  type t = X.t * Y.t

  let pair x y : t = x, y
  let fst (xy : t) = fst xy
  let snd (xy : t) = snd xy
  let inl x = x, Y.id
  let inr y = X.id, y

  let id = X.id, Y.id

  let is_id (x, y) = X.is_id x && Y.is_id y

  let equal (x1, y1) (x2, y2) = X.equal x1 x2 && Y.equal y1 y2

  let lt (x1, y1) (x2, y2) = X.lt x1 x2 || (X.equal x1 x2 && Y.lt y1 y2)

  let leq (x1, y1) (x2, y2) = X.lt x1 x2 || (X.equal x1 x2 && Y.leq y1 y2)

  let compose (x1, y1) (x2, y2) = X.compose x1 x2, Y.compose y1 y2

  let dump fmt (x, y) =
    Format.fprintf fmt "@[<1>(pair@ @[%a@]@ @[%a@])@]" X.dump x Y.dump y
end

module InfiniteProduct (Base : S) :
sig
  include S
  val of_list : Base.t list -> t
  val to_list : t -> Base.t list
end
=
struct
  (* invariants: no ending id *)
  type t = Base.t list

  let rec strip_ids : Base.t list -> t =
    function
    | [] -> []
    | [s] -> if Base.is_id s then [] else [s]
    | s::ss ->
      let ss = strip_ids ss in
      if ss = [] && Base.is_id s then [] else s::ss

  let of_list l : t = strip_ids l
  let to_list (l : t) = l

  let id : t = []

  let is_id (s : t) = s = []

  let rec for_all2 f l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | l1, [] -> List.for_all (fun x1 -> f x1 Base.id) l1
    | [], l2 -> List.for_all (fun x2 -> f Base.id x2) l2
    | x::xs, y::ys -> f x y && for_all2 f xs ys

  let rec exists2 f l1 l2 =
    match l1, l2 with
    | [], [] -> false
    | l1, [] -> List.exists (fun x1 -> f x1 Base.id) l1
    | [], l2 -> List.exists (fun x2 -> f Base.id x2) l2
    | x::xs, y::ys -> f x y || exists2 f xs ys

  let equal l1 l2 = for_all2 Base.equal l1 l2

  let lt l1 l2 = for_all2 Base.leq l1 l2 && exists2 Base.lt l1 l2

  let leq l1 l2 = for_all2 Base.leq l1 l2

  let rec compose l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | l, [] | [], l -> l
    | x::xs, y::ys ->
      let z = Base.compose x y
      and zs = compose xs ys
      in
      if Base.is_id z && zs = [] then [] else z :: zs

  let dump fmt x =
    Format.fprintf fmt "@[<1>[%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";@,") Base.dump)
      x
end

module Prefix (Base : EqualityType) :
sig
  include S
  val prepend : Base.t -> t -> t
end
=
struct
  type t = Base.t list

  let prepend x xs = x :: xs

  let id = []

  let is_id l = l = []

  let equal x y = List.equal Base.equal x y

  let rec lt x y =
    match x, y with
    | [], [] -> false
    | [], _::_ -> true
    | _::_, [] -> false
    | x::xs, y::ys -> Base.equal x y && lt xs ys

  let rec leq x y =
    match x, y with
    | [], _ -> true
    | _::_, [] -> false
    | x::xs, y::ys -> Base.equal x y && leq xs ys

  let compose x y = x @ y

  let dump fmt x =
    Format.fprintf fmt "@[<1>[%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";@,") Base.dump)
      x
end

module Inverted (Base : S) : S
=
struct
  type t = Base.t

  let id = Base.id

  let is_id = Base.is_id

  let equal = Fun.flip Base.equal

  let lt = Fun.flip  Base.lt

  let leq = Fun.flip  Base.leq

  let compose = Base.compose

  let dump = Base.dump
end
