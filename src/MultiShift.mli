open StructuredType

module type BoundedSemilattice =
sig
  include Shift.S

  (** [bot] is the minimum value. *)
  val bot : t

  (** [join x y] is the maximum of [x] and [y]. *)
  val join : t -> t -> t
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
  include BoundedSemilattice with type t = Shift.LexicalPair(X)(Y).t

  val pair : X.t -> Y.t -> t
  val fst : t -> X.t
  val snd : t -> Y.t
  val inl : X.t -> t
  val inr : Y.t -> t
end

(** Level expressions with variables for {!module:Multi} *)
module LiftToExpr (Var : OrderedType) (Base : BoundedSemilattice) :
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

(** Multiple variables *)
module Make (Var : OrderedType) (Base : BoundedSemilattice) :
sig
  (** @closed *)
  include Shift.S

  val join : t -> t -> t
  val of_seq : (Var.t * LiftToExpr(Var)(Base).t) Seq.t -> t
  val to_seq : t -> (Var.t * LiftToExpr(Var)(Base).t) Seq.t
end
