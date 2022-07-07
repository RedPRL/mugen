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

module BinaryProduct (X : Semilattice) (Y : Semilattice) :
sig
  (** @closed *)
  include Semilattice

  val pair : X.t -> Y.t -> t
  val fst : t -> X.t
  val snd : t -> Y.t
  val inl : X.t -> t
  val inr : Y.t -> t
end

module InfiniteProduct (Base : Semilattice) :
sig
  (** @closed *)
  include Semilattice

  val of_list : Base.t list -> t
  val to_list : t -> Base.t list
end

module LexicalBinaryProduct (X : BoundedSemilattice) (Y : BoundedSemilattice) :
sig
  (** @closed *)
  include BoundedSemilattice

  val pair : X.t -> Y.t -> t
  val fst : t -> X.t
  val snd : t -> Y.t
  val inl : X.t -> t
  val inr : Y.t -> t
end

(** Level expressions. *)
module type Expression =
sig
  type var
  type base

  (** @closed *)
  include PartiallyOrderedType

  (** [var v] is the variable [v] as an expression. *)
  val var : var -> t

  (** [subst f e] substitutes every variable [v] in [e] with [f v]. *)
  val subst : (var -> t) -> t -> t

  (** [join x y] is the maximum of [x] and [y]. *)
  val join : t -> t -> t

  (** [const s] is the embedding of [s]. *)
  val const : base -> t

  (** [act s l] applies [s] to the expression [l]. *)
  val act : base -> t -> t
end

(** Substitutions. *)
module MakeEndo (Var : OrderedType) (Base : Semilattice) :
sig
  (** @closed *)
  include Shift.S

  (** Level expressions. *)
  module Expr : Expression with type var := Var.t and type base := Base.t

  val join : t -> t -> t
  val subst : t -> Expr.t -> Expr.t
  val of_seq : (Var.t * Expr.t) Seq.t -> t
  val to_seq : t -> (Var.t * Expr.t) Seq.t
end

(** Both substitutions and levels. *)
module Make (Var : OrderedType) (Base : Semilattice) :
sig
  include Shift.S

  (** Level expressions. *)
  module Expr : Expression with type var := Var.t and type base := Base.t

  val expr : Expr.t -> t
  val of_subst : (Var.t * Expr.t) Seq.t -> t
  val to_either : t -> ((Var.t * Expr.t) Seq.t, Expr.t) Either.t
end
