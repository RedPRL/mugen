module type Semilattice =
sig
  include Shift.S
  val join : t -> t -> t
end

module type BoundedSemilattice =
sig
  include Semilattice
  val bot : t
end

module Int =
struct
  include Shift.Int
  let join x y = of_int (Int.max (to_int x) (to_int y))
end

module Nat =
struct
  include Shift.Nat
  let bot = of_int 0
  let join x y = of_int (Stdlib.Int.max (to_int x) (to_int y))
end

module NonPositive =
struct
  include Shift.NonPositive
  let join x y = of_int (Stdlib.Int.max (to_int x) (to_int y))
end

module BinaryProduct (X : Semilattice) (Y : Semilattice) =
struct
  include Shift.BinaryProduct (X) (Y)

  let join s1 s2 = pair (X.join (fst s1) (fst s2)) (Y.join (snd s1) (snd s2))
end

module LexicalBinaryProduct (X : BoundedSemilattice) (Y : BoundedSemilattice) =
struct
  include Shift.LexicalBinaryProduct (X) (Y)

  let bot = pair X.bot Y.bot

  let join s1 s2 =
    let x = X.join (fst s1) (fst s2) in
    let y1 = if X.equal (fst s1) x then snd s1 else Y.bot
    and y2 = if X.equal (fst s2) x then snd s2 else Y.bot
    in
    pair x (Y.join y1 y2)
end

module NearlyConstant (Base : BoundedSemilattice) :
sig
  include BoundedSemilattice
  val of_based_list : Base.t * Base.t list -> t
  val to_based_list : t -> Base.t * Base.t list
end
=
struct
  include Shift.NearlyConstant (Base)

  (* [of_list] in [join] will do the normalization *)
  let rec join_based_list_ (b1, l1) (b2, l2) =
    match l1, l2 with
    | [], [] -> []
    | l1, [] -> List.map (fun x1 -> Base.join x1 b2) l1
    | [], l2 -> List.map (fun x2 -> Base.join b1 x2) l2
    | x1::l1, x2::l2 -> Base.join x1 x2 :: join_based_list_ (b1, l1) (b2, l2)

  let join bl1 bl2 =
    let b1, l1 = to_based_list bl1
    and b2, l2 = to_based_list bl2
    in
    of_based_list (Base.join b1 b2, join_based_list_ (b1, l1) (b2, l2))

  let bot = of_based_list (Base.bot, [])
end

module FiniteSupport (Base : Semilattice) :
sig
  include Semilattice
  val of_list : Base.t list -> t
  val to_list : t -> Base.t list
end
=
struct
  include Shift.FiniteSupport (Base)

  (* [of_list] in [join] will do the normalization *)
  let rec join_list l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | l, [] -> List.map (fun x -> Base.join x Base.id) l
    | [], l -> List.map (fun x -> Base.join Base.id x) l
    | x::xs, y::ys -> Base.join x y :: join_list xs ys

  let join l1 l2 = of_list (join_list (to_list l1) (to_list l2))
end
