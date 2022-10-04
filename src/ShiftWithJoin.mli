(** A displacement algebra with joins. *)
module type Semilattice =
sig
  (** @closed *)
  include Shift.S

  (** [join x y] is the maximum of [x] and [y]. *)
  val join : t -> t -> t
end

(** A displacement algebra with joins and a bottom element. *)
module type BoundedSemilattice =
sig
  (** @closed *)
  include Semilattice

  (** [bot] is the minimum value. *)
  val bot : t
end

(** Integers with addition. Caveats: it does not handle integer overflow. *)
module Int :
sig
  (** @closed *)
  include Semilattice

  (** Conversion from [int] *)
  val of_int : int -> t

  (** Conversion to [int] *)
  val to_int : t -> int
end

(** Natural numbers with addition. Caveats: it does not handle integer overflow. *)
module Nat :
sig
  (** @closed *)
  include BoundedSemilattice

  (** Conversion from [int] *)
  val of_int : int -> t

  (** Conversion to [int] *)
  val to_int : t -> int
end

(** Non-positive integers with addition. Caveats: it does not handle integer overflow. *)
module NonPositive :
sig
  (** @closed *)
  include Semilattice

  (** Conversion from [int] *)
  val of_int : int -> t

  (** Conversion to [int] *)
  val to_int : t -> int
end

(** Binary products. *)
module Product (X : Semilattice) (Y : Semilattice) :
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

(** Binary products, but with the lexicographical order. *)
module Lexicographic (X : BoundedSemilattice) (Y : BoundedSemilattice) :
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

(** Infinite products with finite elements different from a fixed displacement. *)
module NearlyConstant (Base : BoundedSemilattice) :
sig
  include BoundedSemilattice

  (** Conversion from a based list *)
  val of_based_list : Base.t * Base.t list -> t

  (** Conversion to a based list *)
  val to_based_list : t -> Base.t * Base.t list
end

(** Infinite products with finite supports. A special case of {!module:NearlyConstant}. *)
module FiniteSupport (Base : Semilattice) :
sig
  (** @closed *)
  include Semilattice

  (** Conversion from a list *)
  val of_list : Base.t list -> t

  (** Conversion to a list *)
  val to_list : t -> Base.t list
end
