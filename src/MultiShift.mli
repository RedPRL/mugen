open StructuredType

module type Semilattice =
sig
  (** @closed *)
  include Shift.S

  (** [join x y] is the maximum of [x] and [y]. *)
  val join : t -> t -> t
end

module type BoundedSemilattice =
sig
  (** @closed *)
  include Semilattice

  (** [bot] is the minimum value. *)
  val bot : t
end

module Int :
sig
  (** @closed *)
  include Semilattice

  val of_int : int -> t
  val to_int : t -> int
end

module Nat :
sig
  (** @closed *)
  include BoundedSemilattice

  val of_int : int -> t
  val to_int : t -> int
end

module LexicalPair (X : BoundedSemilattice) (Y : BoundedSemilattice) :
sig
  (** @closed *)
  include BoundedSemilattice

  val pair : X.t -> Y.t -> t
  val fst : t -> X.t
  val snd : t -> Y.t
  val inl : X.t -> t
  val inr : Y.t -> t
end

(** Substitutions. *)
module Make (Var : OrderedType) (Base : Semilattice) :
sig
  (** @closed *)
  include Shift.S

  (** Level expressions. *)
  module Expr :
  sig
    (** @closed *)
    include PartiallyOrderedType

    (** [var v] is the variable [v] as an expression. *)
    val var : Var.t -> t

    (** [subst f e] substitutes every variable [v] in [e] with [f v]. *)
    val subst : (Var.t -> t) -> t -> t

    (** [join x y] is the maximum of [x] and [y]. *)
    val join : t -> t -> t

    (** [const s] is the embedding of [s]. *)
    val const : Base.t -> t

    (** [act s l] applies [s] to the expression [l]. *)
    val act : Base.t -> t -> t
  end

  val join : t -> t -> t
  val of_seq : (Var.t * Expr.t) Seq.t -> t
  val to_seq : t -> (Var.t * Expr.t) Seq.t
end
