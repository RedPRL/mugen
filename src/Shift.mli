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

(** Conor McBride's crude stratification that contains only `f(i) = i + n`. *)
module Crude : S
type crude = Crude.t

(** Generalized {!module:Crude} that allows gaps. *)
module Gapped :
sig

  (** The motivation is to enable insertion of universe levels between any two consecutive levels, which is something
      the function in {!module:Crude} cannot do.
      The idea is to introduce "gaps" in these functions, or in other words, to generalize crude shifting operators
      in {!module:Crude} to {i piecewise} crude shifting operators.
      Technically, we consider all strictly monotone functions [f] such that [f(i) = i + n] when [i >= k] for some finite [k],
      where "gaps" may occur for levels [i < k], which means in-between universe levels can be introduced as levels within the gaps.

      For example, the following shifting operator has three gaps: 0-9, 11-199, and 201-999:
      {v
f(0) = 10
f(1) = 200
f(2) = 1000
f(3) = 1001
f(4) = 1002
...
f(i) = i + 997 (for i >= 3)
...
      v}
      New universe levels that will be made available by this function are 0, 1, ..., 8, 9, 11, 12, ..., 198, 199, 201, 202, ..., 998, and 999.

      The class of gapped functions is closed under identity and composition, and is strictly larger than the class in {!module:Crude}:
      the functions in {!module:Crude} are the functions with [k = 0], or equivalently the functions with at most one gap in the very beginning.
  *)

  include S
  (** @closed *)

  val of_steps : init:int -> int list -> t
  (** [of_steps] can be used to create any shifting operator in this class.
      [of_steps ~init [s0; s1; s2; ...; sn]] is the following function:
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
      As a special case, [of_steps ~init []] is equivalent to [const init].

      @raises Invalid_argument if [init < 0] or [si < 1] for any [i]. *)

  val of_skipped : int list -> t
  (** [of_skipped] can be used to create any shifting operator in this class.
      [of_skipped l] gives the shifting operator that skips the levels in [l]. The numbers in [l] should be non-negative and strictly increasing.
      For example, [of_skipped [0; 1]] is equivalent [const 2] and [of_steps ~init:2 []] because the first two levels are skipped.
      [of_skipped [3]] is equivalent to [of_steps ~init:0 [1;1;2]] because both correspond to the function
      {v
f(0) = 0
f(1) = 1
f(2) = 2
f(3) = 4 (skipping 3)
f(4) = 5
...
      v}
      [of_skipped [0; 1; 2; 4; 5; 7]] is equivalent to [of_steps ~init:3 [3; 2]] because both correspond to the function
      {v
f(0) = 3 (skipping 0, 1, and 2)
f(1) = 6 (skipping 4 and 5)
f(2) = 8 (skipping 7)
f(3) = 9
...
      v}

      @raises Invalid_argument if any element in [l] is not non-negative or if [l] is not strictly increasing.
  *)

  val of_prefix : int list -> t
  (** [of_prefix] can be used to create any shifting operator in this class.
      [of_prefix l] gives the shifting operator whose prefix is [l] and whose gaps are clearly marked in [l].
      As a special case, [of_prefix []] is equivalent to {!val:id}.

      For example, [of_prefix [2]] is equivalent to [of_skipped [0; 1]], [const 2], and [of_steps ~init:2 []].
      [of_prefix [0; 1; 2; 4]] is equivalent to [of_skipped [3]] and [of_steps ~init:0 [1;1;2]].
      [of_prefix [3; 6; 8]] is equivalent to [of_skipped [0; 1; 2; 4; 5; 7]] and [of_steps ~init:3 [3; 2]].

      @raises Invalid_argument if any element in [l] is not non-negative or if [l] is not strictly increasing.
  *)
end
type gapped = Gapped.t
