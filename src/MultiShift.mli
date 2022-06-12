open StructuredType

(** Level expressions with variables for {!module:Multi} *)
module type Expr =
sig
  (** @closed *)
  include PartiallyOrderedType

  (** Type of variables. *)
  type var

  (** [var v] is the variable [v] as an expression. *)
  val var : var -> t

  (** [subst f e] substitutes every variable [v] in [e] with [f v]. *)
  val subst : (var -> t) -> t -> t
end

(** Reduction of multi variables. *)
module Multi (Var : OrderedType) (Expr : Expr with type var := Var.t) :
sig
  (** @closed *)
  include Shift.S

  val of_seq : (Var.t * Expr.t) Seq.t -> t
  val to_seq : t -> (Var.t * Expr.t) Seq.t
end

(** An instance of [Expr] inspired by Agda and Lean. *)
module LiftBoundedSemilattice (Var : OrderedType) (Base : BoundedSemilattice.S) :
sig
  (** @closed *)
  include Expr with type var := Var.t

  (** [bot] is the minimum expression. *)
  val bot : t

  (** [join x y] is the maximum value of [x] and [y]. *)
  val join : t -> t -> t

  (** [const n] represents the constant level [n]. *)
  val const : Base.t -> t

  (** [act s l] applies [s] to the expression [l]. *)
  val act : Base.t -> t -> t
end
