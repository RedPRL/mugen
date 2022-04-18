(** Common interface of classes of shifting operators. *)
module type S =
sig
  (** The type of shifting operators. *)
  type t

  (** [id] is the identity (no shifting). *)
  val id : t

  (** [const n] is the constant shifting [fun i -> i + n].

      @raises Invalid_argument if [n < 0]. *)
  val const : int -> t

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

(** Conor McBride's crude stratification. *)
module Crude : S
type crude = Crude.t

(** Slightly generalized {!module:Crude} that allows finite gaps. *)
module FinSkip :
sig

  (** The idea is to consider all strictly monotone functions [f] such that [f(i) = i + n] when [i >= k] for some finite [k].
      The following is one such function:

      {v
f(0) = 10
f(1) = 200
f(2) = 300
f(3) = 1000
f(4) = 1001
f(5) = 1002
...
f(i) = i + 997 (for i >= 3)
...
      v}

      This class is closed under identity and composition, and is strictly larger than the class in {!module:Crude}
      initially considered by Conor McBride. The functions in {!module:Crude} are the functions with [k = 0].
  *)

  include S
  (** @open *)

  val make : init:int -> steps:int list -> t
  (** [make] can be used to create any function in this class.
      For [steps = [s0; s1; s2; ...; sn]], [make ~init ~steps] is the following function:

      {v
f(0) = init
f(1) = init + s0
f(2) = init + s0 + s1
...
f(i) = init + s0 + s1 + ... + s(i-1)
...
f(n) = init + s0 + s1 + ... + s(n-1)
f(n+1) = init + s0 + s1 + ... + sn
f(n+2) = init + s0 + s1 + ... + sn + 1
f(n+3) = init + s0 + s1 + ... + sn + 2
...
f(n+k+1) = init + s0 + s1 + ... + sn + k
...
      v}

      The [steps] may be empty; [make ~init ~steps:[]] is equivalent to [const init].

      @raises Invalid_argument if [init < 0] or any element in [steps] is [< 1].*)
end
type finskip = FinSkip.t
