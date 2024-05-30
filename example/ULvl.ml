(** A convenience module for freely generated universe level expressions. *)

module Param =
struct
  (** Your chosen displacement algebra *)
  module Shift = Mugen.Shift.Int

  (** The representation of variables in free level expressions *)
  type var = int

  (** The equality checker for variables *)
  let equal_var : var -> var -> bool = Int.equal
end
include Param

(** An alias to the type of displacements *)
type shift = Shift.t

(** An alias to the type of free level expressions *)
type t = (shift, int) Mugen.Syntax.free

(** Smart builders for free level expressions *)
include Mugen.Builder.Free.Make (Param)

(** Comparators for free level expressions *)
include Mugen.Theory.Make (Param)
