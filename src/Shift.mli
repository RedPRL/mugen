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

(** Translation by integers. Caveats: it could not handle integer overflow. *)
module Int :
sig
  (** @closed *)
  include S

  (** [int n] represents the translation [(fun i -> i + n)]. *)
  val int : int -> t
end

module WithConst (Base : S) :
sig
  include S
  val apply : Base.t -> t
  val const : Base.t -> t
end

module type MultiExpr =
sig
  type t
  val var : int -> t
  val subst : (int -> t) -> t -> t
  val equal : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
  val dump : Format.formatter -> t -> unit
end

module Semilattice :
sig
  include MultiExpr
  val nat : int -> t
  val succ : t -> t
  val max : t -> t -> t
end

module Multi (E : MultiExpr) :
sig
  include S
  type expr = E.t
  val singleton : int -> E.t -> t
  val find : int -> t -> expr
  val update : int -> expr -> t -> t
  val of_seq : (int * expr) Seq.t -> t
end

module Fractal (Base : S) :
sig
  include S
  val embed : Base.t -> t
  val push : Base.t -> t -> t
end
