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

module Nat =
struct
  type t = int
  let of_int x = if x < 0 then invalid_arg "Nat.of_int"; x
  let id = 0
  let to_int x = x
  let equal = Stdlib.Int.equal
  let is_id x = x = 0
  let lt : t -> t -> bool = (<)
  let leq : t -> t -> bool = (<=)
  let compose : t -> t -> t = (+)
  let dump = Format.pp_print_int
end

module NonPositive :
sig
  include S
  val of_int : int -> t
  val to_int : t -> int
end
=
struct
  type t = int
  let of_int x = if x > 0 then invalid_arg "NonPositive.of_int"; x
  let id = 0
  let to_int x = x
  let equal = Stdlib.Int.equal
  let is_id x = x = 0
  let lt : t -> t -> bool = (<)
  let leq : t -> t -> bool = (<=)
  let compose : t -> t -> t = (+)
  let dump = Format.pp_print_int
end

module Constant (Act : S) (Const : PartiallyOrderedTypeWithRightAction with type act := Act.t) :
sig
  include S
  val act : Act.t -> t
  val const : Const.t -> t
  val to_either : t -> (Act.t, Const.t) Either.t
end
=
struct
  type t = Act of Act.t | Const of Const.t
  let act x = Act x
  let const x = Const x
  let to_either =
    function
    | Act x -> Either.Left x
    | Const x -> Either.Right x
  let id = act Act.id
  let equal x y =
    match x, y with
    | Act x, Act y -> Act.equal x y
    | Const x, Const y -> Const.equal x y
    | _ -> false
  let is_id = function Act s -> Act.is_id s | _ -> false
  let lt x y =
    match x, y with
    | Act x, Act y -> Act.lt x y
    | Const x, Const y -> Const.lt x y
    | _ -> false
  let leq x y =
    match x, y with
    | Act x, Act y -> Act.leq x y
    | Const x, Const y -> Const.leq x y
    | _ -> false
  let compose x y =
    match x, y with
    | _, Const _ -> y
    | Const x, Act y -> const (Const.act x y)
    | Act x, Act y -> act (Act.compose x y)
  let dump fmt =
    function
    | Const x ->
      Format.fprintf fmt "@[<1>(const@ @[%a@])@]" Const.dump x
    | Act x ->
      Format.fprintf fmt "@[<1>(act@ @[%a@])@]" Act.dump x
end

module BinaryProduct (X : S) (Y : S) :
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

  let lt (x1, y1) (x2, y2) = (X.lt x1 x2 && Y.leq y1 y2) || (X.equal x1 x2 && Y.lt y1 y2)

  let leq (x1, y1) (x2, y2) = X.leq x1 x2 && Y.leq y1 y2

  let compose (x1, y1) (x2, y2) = X.compose x1 x2, Y.compose y1 y2

  let dump fmt (x, y) =
    Format.fprintf fmt "@[<1>(pair@ @[%a@]@ @[%a@])@]" X.dump x Y.dump y
end

module LexicalBinaryProduct (X : S) (Y : S) :
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

module NearlyConstant (Base : S) :
sig
  include S
  val of_based_list : Base.t * Base.t list -> t
  val to_based_list : t -> Base.t * Base.t list
end
=
struct
  (* invariants: no trailing elements equal to base *)
  type t = Base.t * Base.t list

  let rec strip_right base : Base.t list -> Base.t list =
    function
    | [] -> []
    | [s] -> if Base.equal s base then [] else [s]
    | s::ss ->
      let ss = strip_right base ss in
      if ss = [] && Base.equal s base then [] else s::ss

  let of_based_list (b, l) : t = b, strip_right b l
  let to_based_list (l : t) = l

  let id : t = Base.id, []

  let is_id ((b, l) : t) = Base.is_id b && l = []

  let rec for_all2 f (b1, l1) (b2, l2) =
    match l1, l2 with
    | [], [] -> f b1 b2
    | l1, [] -> List.for_all (fun x1 -> f x1 b2) l1
    | [], l2 -> List.for_all (fun x2 -> f b1 x2) l2
    | x1::l1, x2::l2 -> f x1 x2 && for_all2 f (b1, l1) (b2, l2)

  let rec exists2 f (b1, l1) (b2, l2) =
    match l1, l2 with
    | [], [] -> f b1 b2
    | l1, [] -> List.exists (fun x1 -> f x1 b2) l1
    | [], l2 -> List.exists (fun x2 -> f b1 x2) l2
    | x1::l1, x2::l2 -> f x1 x2 || exists2 f (b1, l1) (b2, l2)

  let equal l1 l2 = for_all2 Base.equal l1 l2

  let lt l1 l2 = for_all2 Base.leq l1 l2 && exists2 Base.lt l1 l2

  let leq l1 l2 = for_all2 Base.leq l1 l2

  let rec compose_ (b1, l1) (b2, l2) =
    match l1, l2 with
    | [], [] -> []
    | l1, [] -> List.map (fun x1 -> Base.compose x1 b2) l1
    | [], l2 -> List.map (fun x2 -> Base.compose b1 x2) l2
    | x1::l1, x2::l2 -> Base.compose x1 x2 :: compose_ (b1, l1) (b2, l2)

  let compose (b1, l1) (b2, l2) =
    let b = Base.compose b1 b2 in
    b, strip_right b @@ compose_ (b1, l1) (b2, l2)

  let dump fmt (b, l) =
    Format.fprintf fmt "@[<1>[%a;@,%a...]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";@,") Base.dump) l
      Base.dump b
end

module FiniteSupport (Base : S) :
sig
  include S
  val of_list : Base.t list -> t
  val to_list : t -> Base.t list
end
=
struct
  include NearlyConstant (Base)

  let of_list l = of_based_list (Base.id, l)
  let to_list bl =
    let b, l = to_based_list bl in
    assert (Base.is_id b);
    l
end

module Prefix (Base : EqualityType) :
sig
  include S
  val prepend : Base.t -> t -> t
  val to_list : t -> Base.t list
end
=
struct
  type t = Base.t list

  let prepend x xs = x :: xs

  let to_list xs = xs

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

module Opposite (Base : S) : S with type t = Base.t
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
