(** Smart constructors for {!type:Syntax.endo}. *)
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

    (** [equal_var x y] checks whether two level variables [x] and [y] are the same. *)
    val equal_var : var -> var -> bool
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

    (** [equal l1 l2] checks whether [l1] and [l2] are the same universe level.

        @raise Invalid_argument When [l1] or [l2] is shifted top. *)
    val equal : level -> level -> bool

    (** [lt l1 l2] checks whether [l1] is strictly less than [l2]. Note that trichotomy fails for general universe levels.

        @raise Invalid_argument When [l1] or [l2] is shifted top. *)
    val lt : level -> level -> bool

    (** [leq l1 l2] checks whether [l1] is less than or equal to [l2]. Note that trichotomy fails for general universe levels.

        @raise Invalid_argument When [l1] or [l2] is shifted top. *)
    val leq : level -> level -> bool

    (** [gt l1 l2] is [lt l2 l1]. *)
    val gt : level -> level -> bool

    (** [geq l1 l2] is [leq l2 l1]. *)
    val geq : level -> level -> bool

    (** Infix notation. *)
    module Infix :
    sig
      (** Alias of {!val:equal}. *)
      val (=) : level -> level -> bool

      (** Alias of {!val:lt}. *)
      val (<) : level -> level -> bool

      (** Alias of {!val:leq}. *)
      val (<=) : level -> level -> bool

      (** Alias of {!val:gt}. *)
      val (>) : level -> level -> bool

      (** Alias of {!val:geq}. *)
      val (>=) : level -> level -> bool
    end
  end
end
