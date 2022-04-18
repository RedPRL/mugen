(** Smart constructors for {!type:Syntax.endo}. *)
module Endo :
sig

  (** Parameters of smart constructors. *)
  module type Param =
  sig
    (** The class of shifting operators. *)
    module Shift : Shift.S

    (** The type that embeds levels. *)
    type level

    (** The embedding of levels into [level]. *)
    val level : (Shift.t, level) Syntax.endo -> level

    (** Extract the embedded level, if any. *)
    val unlevel : level -> (Shift.t, level) Syntax.endo option
  end

  (** The signature of smart constructors. *)
  module type S =
  sig
    (** The class of shifting operators. *)
    module Shift : Shift.S

    (** The type that embeds levels. *)
    type level

    (** Smarter version of {!val:Syntax.Endo.shifted} that collapses multiple shifting. *)
    val shifted : level -> Shift.t -> level

    (** [top] is {!val:Syntax.Endo.top} *)
    val top : level

    (** [simplify l] collapses multiple shifting operators and removes useless ones.
        (For example, shifted [top] is still [top].) *)
    val simplify : level -> level

    (** [dissect l] is a helper function that separate the shifting operators from a level (if any).
        If the level is shifted multiple times, the shifting operators are composed into one. *)
    val dissect : level -> level * Shift.t option
  end

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with module Shift := P.Shift and type level = P.level
end

(** Smart constructors for {!type:Syntax.free}. *)
module Free :
sig

  (** Parameters of smart constructors. *)
  module type Param =
  sig
    (** The class of shifting operators. *)
    module Shift : Shift.S

    (** The type of level variables. *)
    type var
  end

  (** The signature of smart constructors. *)
  module type S =
  sig
    (** The class of shifting operators. *)
    module Shift : Shift.S

    (** The type of level variables. *)
    type var

    (** The type of freely generated levels. *)
    type level = (Shift.t, var) Syntax.free

    (** [var] is {!val:Syntax.Free.var} *)
    val var : var -> level
    include Endo.S with module Shift := Shift and type level := level
  end

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S  with module Shift := P.Shift and type var = P.var
end
