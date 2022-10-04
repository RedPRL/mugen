open StructuredType

(** A displacement algebra. *)
module type S =
sig
  (** To form a valid displacement algebra, {!val:compose} and {!val:id} should form a monoid, and {!val:lt} (the strict order) must be left-invariant respect to {!val:compose}. *)

  (** @open *)
  include PartiallyOrderedType

  (** [id] is the unit. *)
  val id : t

  (** [is_id s] checks whether [s] is the unit. It is equivalent to [equal id s], but potentially faster. *)
  val is_id : t -> bool

  (** [compose s1 s2] composes the operators [s1] and [s2]. Note that [Foo^s1^s2] in McBride's notation is understood as [compose (compose ... s2) s1] with the reversed order. *)
  val compose : t -> t -> t
end

(** Integers with addition. Caveats: it does not handle integer overflow. *)
module Int :
sig
  (** @closed *)
  include S

  (** Conversion from [int] *)
  val of_int : int -> t

  (** Conversion to [int] *)
  val to_int : t -> int
end

(** Natural numbers with addition. Caveats: it does not handle integer overflow. *)
module Nat :
sig
  (** @closed *)
  include S

  (** Conversion from [int] *)
  val of_int : int -> t

  (** Conversion to [int] *)
  val to_int : t -> int
end

(** Non-positive integers with addition. Caveats: it does not handle integer overflow. *)
module NonPositive :
sig
  (** @closed *)
  include S

  (** Conversion from [int] *)
  val of_int : int -> t

  (** Conversion to [int] *)
  val to_int : t -> int
end

(** Constant displacements. *)
module Constant (Act : S) (Const : PartiallyOrderedTypeWithRightAction with type act := Act.t) :
sig
  (** @closed *)
  include S

  (** [act s] represents actions. *)
  val act : Act.t -> t

  (** [const s] represents constants. *)
  val const : Const.t -> t

  (** [to_either] convert an element to a value of type [Either.t] *)
  val to_either : t -> (Act.t, Const.t) Either.t
end

(** Binary products. *)
module Product (X : S) (Y : S) :
sig
  (** @closed *)
  include S

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
module Lexicographic (X : S) (Y : S) :
sig
  (** @closed *)
  include S

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
module NearlyConstant (Base : S) :
sig
  (** @closed *)
  include S

  (** Conversion from a based list; a based list [(b, l)] represents the following infinite product (as a function from natural numbers to [Base.t])
      {v
f 0 = List.nth l 0
f 1 = List.nth l 1
...
f (n-1) = List.nth l (n-1)
f n = b
f (n+1) = b
...
      v}
  *)
  val of_based_list : Base.t * Base.t list -> t

  (** Right inverse of {!val:of_based_list}. It is not a left inverse of {!val:of_based_list} because trailing values that are equal to the base will be stripped. *)
  val to_based_list : t -> Base.t * Base.t list
end

(** Infinite products with finite supports. A special case of {!module:NearlyConstant} where the base is [id]. *)
module FiniteSupport (Base : S) :
sig
  (** @closed *)
  include S

  (** Conversion from a list; a list [l] represents the following infinite product (as a function from natural numbers to [Base.t])
      {v
f 0 = List.nth l 0
f 1 = List.nth l 1
...
f (n-1) = List.nth l (n-1)
f n = Base.id
f (n+1) = Base.id
...
      v}
  *)
  val of_list : Base.t list -> t

  (** List representation of an infinite product. Right inverse of {!val:of_list}. It is not a left inverse of {!val:of_list} because trailing [Base.id] will be stripped. *)
  val to_list : t -> Base.t list
end

(** Prefix displacements. *)
module Prefix (Base : EqualityType) :
sig
  (** @closed *)
  include S

  (** Prepend a symbol to a displacement. *)
  val prepend : Base.t -> t -> t

  (** Conversion to a list *)
  val to_list : t -> Base.t list
end

(** Fractal displacements. *)
module Fractal (Base : S) :
sig
  (** @closed *)
  include S

  (** [embed b] is the embedding of the base displacement [b]. *)
  val embed : Base.t -> t

  (** [push b s] pushes [s] to the sub-level and applies [b] to the main level. *)
  val push : Base.t -> t -> t
end

(** Opposite displacements *)
module Opposite (Base : S) :
sig
  include S

  (** [of_base b] gives the same level [b] in the opposite algebra. *)
  val of_base : Base.t -> t

  (** [to_base b] gives the same level [b] in the original algebra. *)
  val to_base : t -> Base.t
end
