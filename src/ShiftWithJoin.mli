(** A displacement ordered monoid with joins. *)
module type Semilattice =
sig
  (** @closed *)
  include Shift.S

  (** [join x y] is the maximum of [x] and [y]. *)
  val join : t -> t -> t
end

(** A displacement ordered monoid with joins and a bottom element. *)
module type BoundedSemilattice =
sig
  (** @closed *)
  include Semilattice

  (** [bot] is the minimum value. *)
  val bot : t
end

(** The displacement algebra of integers. *)
module Int :
sig
  (** @closed *)
  include Semilattice

  (** Conversion from [int] *)
  val of_int : int -> t

  (** Conversion to [int] *)
  val to_int : t -> int
end

(** The displacement algebra of natural numbers. *)
module Nat :
sig
  (** @closed *)
  include BoundedSemilattice

  (** Conversion from [int] *)
  val of_int : int -> t

  (** Conversion to [int] *)
  val to_int : t -> int
end

(** The displacement algebra of non-positive integers. *)
module NonPos :
sig
  (** @closed *)
  include Semilattice

  (** Conversion from [int] *)
  val of_int : int -> t

  (** Conversion to [int] *)
  val to_int : t -> int
end

(** The binary product of two displacement semilattices. *)
module BinaryProduct (X : Semilattice) (Y : Semilattice) :
sig
  (** @closed *)
  include Semilattice

  (** Forming a pair *)
  val pair : X.t -> Y.t -> t

  (** First projection *)
  val fst : t -> X.t

  (** Second projection *)
  val snd : t -> Y.t

  (** [inl x] is equivalent to [pair x Y.id] *)
  val inl : X.t -> t

  (** [inr y] is equivalent to [pair X.id y] *)
  val inr : Y.t -> t
end

(** The binary product of two bounded displacement semilattices, but with the lexicographical order. *)
module LexicalBinaryProduct (X : BoundedSemilattice) (Y : BoundedSemilattice) :
sig
  (** @closed *)
  include BoundedSemilattice

  (** Forming a pair *)
  val pair : X.t -> Y.t -> t

  (** First projection *)
  val fst : t -> X.t

  (** Second projection *)
  val snd : t -> Y.t

  (** [inl x] is equivalent to [pair x Y.id] *)
  val inl : X.t -> t

  (** [inr y] is equivalent to [pair X.id y] *)
  val inr : Y.t -> t
end

(** The infinite product of a displacement semilattice. *)
module InfiniteProduct (Base : Semilattice) :
sig
  (** @closed *)
  include Semilattice

  (** Conversion from a list *)
  val of_list : Base.t list -> t

  (** Conversion to a list *)
  val to_list : t -> Base.t list
end
