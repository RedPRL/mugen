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

  (** [shifted s l] is [Shifted (l, s)]. *)
  val shifted : 'a -> 's -> ('s, 'a) t

  (** [top] is [Top]. *)
  val top : ('s, 'a) t

  (** Ugly printer. *)
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

  (** [shifted s l] is [Level (Shifted (l, s))]. *)
  val shifted : ('s, 'v) t -> 's -> ('s, 'v) t

  (** [top] is [Level Top]. *)
  val top : ('s, 'v) t

  (** [var v] is [Var v]. *)
  val var : 'v -> ('s, 'v) t

  (** Ugly printer. *)
  val dump :
    (Format.formatter -> 's -> unit) ->
    (Format.formatter -> 'v -> unit) ->
    Format.formatter -> ('s, 'v) t -> unit
end
