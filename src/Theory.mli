(** Parameters of universe level comparators. *)
module type Param = TheorySigs.Param

(** The signature of universe level comparators. *)
module type S = TheorySigs.S

(** The universe level comparator. *)
module Make (P : Param) : S with type shift := P.Shift.t and type var := P.var
