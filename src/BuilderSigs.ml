(** Smart constructors for {!type:Syntax.endo}. *)
module Endo =
struct

  (** Parameters of smart constructors. *)
  module type Param =
  sig
    (** The displacement algebra. *)
    module Shift : Shift.S

    (** The type that embeds level expressions. *)
    type level

    (** The embedding of level expressions into {!type:level}. *)
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

    (** [shifted s l] is the smarter version of {!val:Syntax.Endo.shifted} that collapses multiple displacements,
        representing the level [l] shifted by the displacement [s].

        @raise Invalid_argument When it (directly or indirectly) attempts to shift the top level. *)
    val shifted : level -> shift -> level

    (** [top] is {!val:Syntax.Endo.top}, the additional top level for convenience. *)
    val top : level
  end
end

(** Smart constructors for {!type:Syntax.free}. *)
module Free =
struct

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

    (** [var] is {!val:Syntax.Free.var}, representing the variable level [v]. *)
    val var : var -> level

    include Endo.S with type shift := shift and type level := level
  end
end
