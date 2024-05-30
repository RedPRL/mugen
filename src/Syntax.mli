(** A family of polynomial endofunctors [('s, -) endo] indexed by the type of displacements ['s]. *)
type ('s, 'a) endo =
  | Shifted of 'a * 's
  | Top

(** The free monad [('s, -) free] on the endofunctor [('s, -) endo] indexed by the type of displacements ['s]. *)
type ('s, 'v) free =
  | Level of ('s, ('s, 'v) free) endo
  | Var of 'v

(** Stupid constructors for {!type:endo}. *)
module Endo :
sig
  (** A family of polynomial endofunctors [('s, -) t] indexed by the type of displacements ['s]. *)
  type ('s, 'a) t = ('s, 'a) endo =
    | Shifted of 'a * 's
    | Top

  (** [shifted l s] is [Shifted (l, s)], representing the level [l] shifted by the displacement [s]. *)
  val shifted : 'a -> 's -> ('s, 'a) t

  (** [top] is [Top], the additional top level for convenience. *)
  val top : ('s, 'a) t

  (** [dump dump_s dump_a] is the ugly printer for levels, where [dump_s] is the printer for displacements and [dump_a] is the printer for inner sub-expressions. *)
  val dump :
    (Format.formatter -> 's -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> ('s, 'a) t -> unit
end

(** Stupid constructors for {!type:free}. *)
module Free :
sig
  (** The free monad [('s, -) t] on the endofunctor [('s, -) endo] indexed by the type of displacements ['s]. *)
  type ('s, 'v) t = ('s, 'v) free =
    | Level of ('s, ('s, 'v) free) endo
    | Var of 'v

  (** [shifted l s] is [Level (Shifted (l, s))], representing the level [l] shifted by the displacement [s]. *)
  val shifted : ('s, 'v) t -> 's -> ('s, 'v) t

  (** [top] is [Top], the additional top level for convenience. *)
  val top : ('s, 'v) t

  (** [var v] is [Var v], representing the variable level [v]. *)
  val var : 'v -> ('s, 'v) t

  (** [dump dump_s dump_v] is the ugly printer for levels, where [dump_s] is the printer for displacements and [dump_v] is the printer for variables. *)
  val dump :
    (Format.formatter -> 's -> unit) ->
    (Format.formatter -> 'v -> unit) ->
    Format.formatter -> ('s, 'v) t -> unit
end
