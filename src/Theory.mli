(** Parameters of universe level comparators. *)
module type Param =
sig
  (** The class of shifting operators. It is assumed that [Shift.compose] is associative. *)
  module Shift : Shift.S

  (** The type of level variables. *)
  type var

  (** [equal_var x y] checks whether two level variables [x] and [y] are the same. *)
  val equal_var : var -> var -> bool
end

(** The signature of universe level comparators. *)
module type S =
sig
  (** The type of shifting operators. *)
  type shift

  (** The type of level variables. *)
  type var

  (** The type of freely generated levels. *)
  type level = (shift, var) Syntax.free

  (** [equal l1 l2] checks whether [l1] and [l2] are the same universe level. *)
  val equal : level -> level -> bool

  (** [lt l1 l2] checks whether [l1] is strictly less than [l2]. Note that trichotomy fails for general universe levels. *)
  val lt : level -> level -> bool

  (** [leq l1 l2] checks whether [l1] is less than or equal to [l2]. Note that trichotomy fails for general universe levels. *)
  val leq : level -> level -> bool

  (** [gt l1 l2] is [lt l2 l1]. *)
  val gt : level -> level -> bool

  (** [geq l1 l2] is [leq l2 l1]. *)
  val geq : level -> level -> bool

  (** Alias of {!val:equal}. *)
  val (=) : level -> level -> bool

  (** Alias of {!val:lt}. *)
  val (<) : level -> level -> bool

  (** Alias of {!val:le}. *)
  val (<=) : level -> level -> bool

  (** Alias of {!val:gt}. *)
  val (>) : level -> level -> bool

  (** Alias of {!val:ge}. *)
  val (>=) : level -> level -> bool
end

(** The universe level comparator. *)
module Make (P : Param) : S with type shift := P.Shift.t and type var := P.var
