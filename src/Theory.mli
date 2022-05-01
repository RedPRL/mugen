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

  (** [le l1 l2] checks whether [l1] is less than or equal to [l2]. Note that trichotomy fails for general universe levels. *)
  val le : level -> level -> bool

  (** [gt l1 l2] is [lt l2 l1]. *)
  val gt : level -> level -> bool

  (** [ge l1 l2] is [le l2 l1]. *)
  val ge : level -> level -> bool

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
