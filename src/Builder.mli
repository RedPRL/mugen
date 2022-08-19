(** Smart constructors for {!type:Syntax.endo}. *)
module Endo :
sig

  (** Parameters of smart constructors. *)
  module type Param =
  sig
    (** The displacement algebra. *)
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
    (** The displacement algebra. *)
    type shift

    (** The type that embeds levels. *)
    type level

    (** Smarter version of {!val:Syntax.Endo.shifted} that collapses multiple displacements

        @raise Invalid_argument When it attempts to shift the top level. *)
    val shifted : level -> shift -> level

    (** [top] is {!val:Syntax.Endo.top} *)
    val top : level

    (** [simplify l] collapses multiple displacements and removes useless ones.
        (For example, shifted [top] is just [top].)

        @raise Invalid_argument When it attempts to shift the top level. *)
    val simplify : level -> level

    (** [dissect l] is a helper function that extract displacements from a level (if any).
        If the level is displaced multiple times, all the displacements are composed into one.

        @raise Invalid_argument When it attempts to shift the top level. *)
    val dissect : level -> level * shift option
  end

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type shift := P.Shift.t and type level := P.level
end

(** Smart constructors for {!type:Syntax.free}. *)
module Free :
sig

  (** Parameters of smart constructors. *)
  module type Param =
  sig
    (** The displacement algebra. *)
    module Shift : Shift.S

    (** The type of level variables. *)
    type var
  end

  (** The signature of smart constructors. *)
  module type S =
  sig
    (** The displacement algebra. *)
    type shift

    (** The type of level variables. *)
    type var

    (** The type of freely generated levels. *)
    type level = (shift, var) Syntax.free

    (** [var] is {!val:Syntax.Free.var} *)
    val var : var -> level

    include Endo.S with type shift := shift and type level := level
  end

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type shift := P.Shift.t and type var := P.var
end
