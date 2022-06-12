module type S =
sig
  include Shift.S
  val bot : t
  val join : t -> t -> t
end

module Nat =
struct
  type t = Shift.Int.t
  let of_int x = if x < 0 then invalid_arg "BoundedSemilattice.Nat.of_int"; Shift.Int.of_int x
  let bot = of_int 0
  let id = Shift.Int.id
  let to_int = Shift.Int.to_int
  let equal = Shift.Int.equal
  let is_id = Shift.Int.is_id
  let lt = Shift.Int.lt
  let leq = Shift.Int.leq
  let compose = Shift.Int.compose
  let join x y = of_int (Int.max (to_int x) (to_int y))
  let dump = Shift.Int.dump
end

module LexicalPair (X : S) (Y : S) =
struct
  include Shift.LexicalPair (X) (Y)

  let bot = pair X.bot Y.bot

  let join x y =
    let join_fst = X.join (fst x) (fst y) in
    let join_snd =
      match X.equal (fst x) join_fst, X.equal (fst y) join_fst with
      | false, false -> Y.bot
      | true, false -> snd x
      | false, true -> snd y
      | true, true -> Y.join (snd x) (snd y)
    in
    pair join_fst join_snd
end
