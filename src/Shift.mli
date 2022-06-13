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

(** Augmentation with constants. *)
module WithConst (Base : S) :
sig
  (** @closed *)
  include S

  (** [act s] represents [(fun f i -> f (s i))]. *)
  val act : Base.t -> t

  (** [const s] represents [(fun f _ -> f (s id))]. *)
  val const : Base.t -> t
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

(** Pairs using the lexicographical order. *)
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

(** The infinite product of the base class. *)
module InfiniteProduct (Base : S) :
sig
  (** @closed *)
  include S

  val of_list : Base.t list -> t
  val to_list : t -> Base.t list
end

(** Prefix order. *)
module Prefix (Base : EqualityType) :
sig
  (** @closed *)
  include S

  val prepend : Base.t -> t -> t
end
