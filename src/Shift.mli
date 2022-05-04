(** Common interface of classes of shifting operators. *)
module type S =
sig
  (** The type of shifting operators. *)
  type t

  (** [id] is the identity (no shifting). *)
  val id : t

  (** [trans n] is the translation [fun i -> i + n].

      @raises Invalid_argument if [n < 0]. *)
  val trans : int -> t

  (** [is_id s] checkes whether [s] is the identity. *)
  val is_id : t -> bool

  (** [equal x y] checks whether [x] and [y] are the same operator. *)
  val equal : t -> t -> bool

  (** [lt x y] checks if [x] is strictly less than [y]. Note that trichotomy in general fails for shifting operators. *)
  val lt : t -> t -> bool

  (** [le x y] checks if [x] is less than or equal to [y]. Note that trichotomy in general fails for shifting operators. *)
  val le : t -> t -> bool

  (** [compose s0 s1] composes the operators [s0] and [s1], in the texual order. *)
  val compose : t -> t -> t

  (** Ugly printer. *)
  val dump : Format.formatter -> t -> unit
end

(** Translation. *)
module Trans : S
type trans = Trans.t

(** Translation and scaling. *)
module Linear :
sig
  (** The motivation is to enable insertion of universe levels between any two consecutive levels, which is something
      the function in {!module:Crude} cannot do.
      The idea is to introduce scaling so that we are considering [fun i -> i * n1 + n0] instead of just [fun i -> i + n].
  *)

  (** @closed *)
  include S

  (** [scale n] is the scaling [fun i -> i * n]. *)
  val scale : int -> t
end
type linear = Linear.t

(** Translation, scaling, and post increments. *)
module LinearPostInc :
sig
  (** The motivation is to enable insertion of universe levels between any two consecutive levels, which is something
      the function in {!module:Crude} cannot do.
      The idea is to introduce scaling and post increments so that we are considering the higher-order functions
      [fun f i -> f (i * n1 + n0) + n2] instead of just [fun i -> i + n].

      The function [g] from levels to levels is embedded into the class of higher-order functions as [fun f i -> f (g i)].
  *)

  (** @closed *)
  include S

  (** [scale n] is the scaling [fun f i -> f (i * n)] (the embedding of [fun i -> i * n]). *)
  val scale : int -> t

  (** [postinc n] is the post increment [fun f i -> f i + n]. *)
  val postinc : int -> t
end
type lpi = LinearPostInc.t

(** Class of dual translation. *)
module DualTrans :
sig
  (** @closed *)
  include S

  (** Secondary translation. *)
  val secondary_trans : int -> t
end
type dual_trans = DualTrans.t
