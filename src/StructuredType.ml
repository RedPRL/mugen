module type EqualityType =
sig
  (** The type. *)
  type t

  (** [equal x y] checks whether [x] and [y] are equivalent. *)
  val equal : t -> t -> bool

  (** Ugly printer. *)
  val dump : Format.formatter -> t -> unit
end

module type PartiallyOrderedType =
sig
  (** @closed *)
  include EqualityType

  (** [lt x y] checks if [x] is strictly less than [y]. Note that trichotomy fails for general partial orders. *)
  val lt : t -> t -> bool

  (** [leq x y] checks if [x] is less than or equal to [y]. Note that trichotomy fails for general partial orders. *)
  val leq : t -> t -> bool
end
