(** Parameters of the theory. *)
module type Param = TheorySigs.Param

(** The signature of the theory. *)
module type S = TheorySigs.S

(** The implementation of the theory. *)
module Make (P : Param) : S with type shift := P.Shift.t and type var := P.var
