(** Parameters of cofibration solvers. *)
module type Param =
sig
  module Shift : Shift.S
  type var
  val equal_var : var -> var -> bool
end

(** The signature of cofibration solvers. *)
module type S =
sig
  module Shift : Shift.S
  type var
  type level = (Shift.t, var) Syntax.free
  val equal : level -> level -> bool
  val lt : level -> level -> bool
  val le : level -> level -> bool
end

(** The cofibration solver. *)
module Make (P : Param) : S with module Shift = P.Shift and type var = P.var
