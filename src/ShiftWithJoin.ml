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

module Nat =
struct
  type t = int
  let of_int x = if x < 0 then invalid_arg "MultiShift.Nat.of_int"; x
  let bot = 0
  let id = 0
  let to_int x = x
  let equal = Stdlib.Int.equal
  let is_id x = x = 0
  let lt : t -> t -> bool = (<)
  let leq : t -> t -> bool = (<=)
  let compose : t -> t -> t = (+)
  let join : t -> t -> t = Stdlib.Int.max
  let dump = Format.pp_print_int
end

module Int =
struct
  include Shift.Int
  let join x y = of_int (Int.max (to_int x) (to_int y))
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

module InfiniteProduct (Base : Semilattice) :
sig
  include Semilattice
  val of_list : Base.t list -> t
  val to_list : t -> Base.t list
end
=
struct
  include Shift.InfiniteProduct (Base)

  (* [of_list] in [join] will do the normalization *)
  let rec join_list l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | l, [] -> List.map (fun x -> Base.join x Base.id) l
    | [], l -> List.map (fun x -> Base.join Base.id x) l
    | x::xs, y::ys -> Base.join x y :: join_list xs ys

  let join l1 l2 = of_list (join_list (to_list l1) (to_list l2))
end
