(** Semantic operations for {!type:Syntax.endo}. *)
module Endo :
sig

  (** Parameters of smart constructors. *)
  module type Param = BuilderSigs.Endo.Param

  (** The signature of smart constructors. *)
  module type S = BuilderSigs.Endo.S

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type shift := P.Shift.t and type level := P.level
end

(** Semantic operations for {!type:Syntax.free}. *)
module Free :
sig

  (** Parameters of smart constructors. *)
  module type Param = BuilderSigs.Free.Param

  (** The signature of smart constructors. *)
  module type S = BuilderSigs.Free.S

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type shift := P.Shift.t and type var := P.var
end
