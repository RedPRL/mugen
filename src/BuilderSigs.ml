(** Smart builders for {!type:Syntax.endo}. *)
module Endo =
struct

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
  end
end

(** Smart builders for {!type:Syntax.free}. *)
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

    (** [var] is {!val:Syntax.Free.var} *)
    val var : var -> level

    include Endo.S with type shift := shift and type level := level
  end
end
