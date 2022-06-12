module type S =
sig
  include Shift.S
  val bot : t
  val join : t -> t -> t
end

module Nat :
sig
  (** @closed *)
  include S

  val of_int : int -> t
  val to_int : t -> int
end

module LexicalPair (X : S) (Y : S) :
sig
  (** @closed *)
  include S

  val pair : X.t -> Y.t -> t
  val fst : t -> X.t
  val snd : t -> Y.t
  val inl : X.t -> t
  val inr : Y.t -> t
end
