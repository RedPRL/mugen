module type Semilattice =
sig
  (** @closed *)
  include Shift.S

  (** [join x y] is the maximum of [x] and [y]. *)
  val join : t -> t -> t
end

module type BoundedSemilattice =
sig
  (** @closed *)
  include Semilattice

  (** [bot] is the minimum value. *)
  val bot : t
end

module Int :
sig
  (** @closed *)
  include Semilattice

  val of_int : int -> t
  val to_int : t -> int
end

module Nat :
sig
  (** @closed *)
  include BoundedSemilattice

  val of_int : int -> t
  val to_int : t -> int
end

module BinaryProduct (X : Semilattice) (Y : Semilattice) :
sig
  (** @closed *)
  include Semilattice

  val pair : X.t -> Y.t -> t
  val fst : t -> X.t
  val snd : t -> Y.t
  val inl : X.t -> t
  val inr : Y.t -> t
end

module InfiniteProduct (Base : Semilattice) :
sig
  (** @closed *)
  include Semilattice

  val of_list : Base.t list -> t
  val to_list : t -> Base.t list
end

module LexicalBinaryProduct (X : BoundedSemilattice) (Y : BoundedSemilattice) :
sig
  (** @closed *)
  include BoundedSemilattice

  val pair : X.t -> Y.t -> t
  val fst : t -> X.t
  val snd : t -> Y.t
  val inl : X.t -> t
  val inr : Y.t -> t
end
