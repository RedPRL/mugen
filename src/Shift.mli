(** Common interface of classes of shifting operators. *)
module type S =
sig
  (** The type of shifting operators. *)
  type t

  (** [id] is the identity (no shifting). *)
  val id : t

  (** [equal x y] checks whether [x] and [y] are equivalent. *)
  val equal : t -> t -> bool

  (** [is_id s] checks whether [s] is the identity. It is equivalent to [equal id s], but potentially faster. *)
  val is_id : t -> bool

  (** [lt x y] checks if [x] is strictly less than [y]. Note that trichotomy fails for general partial orders. *)
  val lt : t -> t -> bool

  (** [leq x y] checks if [x] is less than or equal to [y]. Note that trichotomy fails for general partial orders. *)
  val leq : t -> t -> bool

  (** [compose s0 s1] composes the operators [s0] and [s1]. Note that [Foo^k1^k2] in McBride's notation is understood as [compose (compose ... k2) k1]. *)
  val compose : t -> t -> t

  (** Ugly printer. *)
  val dump : Format.formatter -> t -> unit
end

(** Translation by integers. Caveats: it does not handle integer overflow. *)
module Int :
sig
  (** @closed *)
  include S

  (** [int n] represents the translation [(fun i -> i + n)]. *)
  val int : int -> t
end

(** Augmentation with constants. *)
module WithConst (Base : S) :
sig
  (** @closed *)
  include S

  (** [apply s] represents [(fun f i -> f (s i))]. *)
  val apply : Base.t -> t

  (** [const s] represents [(fun f _ -> f (s id))]. *)
  val const : Base.t -> t
end

(** Level expressions with variables for {!module:MultiExpr} *)
module type MultiExpr =
sig
  (** Type of variables. *)
  type var

  (** Type of expressions. *)
  type t

  (** [var v] is the variable [v] as an expression. *)
  val var : var -> t

  (** [subst f e] substitutes every variable [v] in [e] with [f v]. *)
  val subst : (var -> t) -> t -> t

  (** [equal x y] returns [true] if [x] and [y] are equivalent experssions. *)
  val equal : t -> t -> bool

  (** [lt x y] returns [true] if [x] is strictly less than [y]. *)
  val lt : t -> t -> bool

  (** [leq x y] returns [true] if [x] is less than or equal to [y]. *)
  val leq : t -> t -> bool

  (** Ugly printer for expressions. *)
  val dump : Format.formatter -> t -> unit
end

module type OrderedType =
sig
  (** @closed *)
  include Map.OrderedType

  (** Ugly printer. *)
  val dump : Format.formatter -> t -> unit
end

(** An instance of [MultiExpr] inspired by Agda and Lean. *)
module Semilattice (Var : OrderedType) :
sig
  (** @closed *)
  include MultiExpr

  (** [nat n] represents the constant level [n]. *)
  val nat : int -> t

  (** [succ e] is the successor of [e]. *)
  val succ : t -> t

  (** [max x y] is the maximum value of [x] and [y]. *)
  val max : t -> t -> t
end

(** Reduction of multi variables. *)
module Multi (V : OrderedType) (E : MultiExpr with type var = V.t) :
sig
  (** @closed *)
  include S

(** type of expressions *)
  type expr = E.t

  (** One-variable substitution. *)
  val singleton : V.t -> E.t -> t

  (** Fetch the substitution. *)
  val find : V.t -> t -> expr

  (** Update the substitution. *)
  val update : V.t -> expr -> t -> t

  (** Create a multi-variable substitution. *)
  val of_seq : (V.t * expr) Seq.t -> t
end

(** Fractal universe levels. *)
module Fractal (Base : S) :
sig
  include S

  (** [embed b] is the embedding of the base shift [b]. *)
  val embed : Base.t -> t

  (** [push b s] pushes [s] to the sub-level and applies [b] to the main level. *)
  val push : Base.t -> t -> t
end
