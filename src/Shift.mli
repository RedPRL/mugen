open StructuredType

(** Common interface of classes of shifting operators. *)
module type S =
sig
  (** @open *)
  include PartiallyOrderedType

  (** [id] is the identity (no shifting). *)
  val id : t

  (** [is_id s] checks whether [s] is the identity. It is equivalent to [equal id s], but potentially faster. *)
  val is_id : t -> bool

  (** [compose s1 s2] composes the operators [s1] and [s2]. Note that [Foo^s1^s2] in McBride's notation is understood as [compose (compose ... s2) s1] with the reversed order. *)
  val compose : t -> t -> t
end

(** Integers with addition as composition. Caveats: it does not handle integer overflow. *)
module Int :
sig
  (** @closed *)
  include S

  val of_int : int -> t
  val to_int : t -> int
end

module type RightAction =
sig
  (** @closed *)
  include PartiallyOrderedType

  (** the underlying type of a displacement algebra *)
  type act

  (** the type of right displacement actions *)
  val act : t -> act -> t
end

(** Augmentation with constants. *)
module Constant (Base : S) (Const : RightAction with type act := Base.t) :
sig
  (** @closed *)
  include S

  (** [act s] represents actions. *)
  val act : Base.t -> t

  (** [const s] represents constants. *)
  val const : Const.t -> t

  (** [to_either] convert an element to a value of type [Either.t] *)
  val to_either : t -> (Base.t, Const.t) Either.t
end

(** Fractal universe levels. *)
module Fractal (Base : S) :
sig
  (** @closed *)
  include S

  (** [embed b] is the embedding of the base shift [b]. *)
  val embed : Base.t -> t

  (** [push b s] pushes [s] to the sub-level and applies [b] to the main level. *)
  val push : Base.t -> t -> t
end

(** Binary products. *)
module BinaryProduct (X : S) (Y : S) :
sig
  (** @closed *)
  include S

  val pair : X.t -> Y.t -> t
  val fst : t -> X.t
  val snd : t -> Y.t
  val inl : X.t -> t
  val inr : Y.t -> t
end

(** The infinite product of the base class. *)
module InfiniteProduct (Base : S) :
sig
  (** @closed *)
  include S

  val of_list : Base.t list -> t
  val to_list : t -> Base.t list
end

(** Binary products but using the lexicographical order. *)
module LexicalBinaryProduct (X : S) (Y : S) :
sig
  (** @closed *)
  include S

  val pair : X.t -> Y.t -> t
  val fst : t -> X.t
  val snd : t -> Y.t
  val inl : X.t -> t
  val inr : Y.t -> t
end

(** Prefix order. *)
module Prefix (Base : EqualityType) :
sig
  (** @closed *)
  include S

  val prepend : Base.t -> t -> t
end
