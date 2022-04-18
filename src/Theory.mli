(** Parameters of universe level comparators. *)
module type Param =
sig
  (** The class of shifting operators. *)
  module Shift : Shift.S

  (** The type of level variables. *)
  type var

  (** [equal_var x y] checks whether two level variables [x] and [y] are the same. *)
  val equal_var : var -> var -> bool
end

(** The signature of universe level comparators. *)
module type S =
sig
  (** The class of shifting operators. *)
  module Shift : Shift.S

  (** The type of level variables. *)
  type var

  (** The type of freely generated levels. *)
  type level = (Shift.t, var) Syntax.free

  (** [equal l1 l2] checks whether [l1] and [l2] are the same universe level. *)
  val equal : level -> level -> bool

  (** [lt l1 l2] checks whether [l1] is strictly less than [l2]. Note that trichotomy fails for general universe levels. *)
  val lt : level -> level -> bool

  (** [le l1 l2] checks whether [l1] is less than or equal to [l2]. Note that trichotomy fails for general universe levels. *)
  val le : level -> level -> bool
end

(** The universe level comparator. *)
module Make (P : Param) : S with module Shift = P.Shift and type var = P.var
