(** Smart constructors for {!type:Syntax.endo}. *)
module Endo :
sig

  (** Parameters of smart constructors. *)
  module type Param = TheorySigs.Endo.Param

  (** The signature of smart constructors. *)
  module type S = TheorySigs.Endo.S

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type shift := P.Shift.t and type level := P.level
end

(** Smart constructors for {!type:Syntax.free}. *)
module Free :
sig

  (** Parameters of smart constructors. *)
  module type Param = TheorySigs.Free.Param

  (** The signature of smart constructors. *)
  module type S = TheorySigs.Free.S

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type shift := P.Shift.t and type var := P.var
end
