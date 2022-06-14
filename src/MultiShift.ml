open StructuredType

module type Semilattice =
sig
  include Shift.S
  val join : t -> t -> t
end

module type BoundedSemilattice =
sig
  include Semilattice
  val bot : t
end

module Int =
struct
  include Shift.Int
  let join x y = of_int (Int.max (to_int x) (to_int y))
end

module Nat =
struct
  type t = int
  let of_int x = if x < 0 then invalid_arg "MultiShift.Nat.of_int"; x
  let bot = 0
  let id = 0
  let to_int x = x
  let equal = Stdlib.Int.equal
  let is_id x = x = 0
  let lt : t -> t -> bool = (<)
  let leq : t -> t -> bool = (<=)
  let compose : t -> t -> t = (+)
  let join : t -> t -> t = Stdlib.Int.max
  let dump = Format.pp_print_int
end

module LexicalPair (X : BoundedSemilattice) (Y : BoundedSemilattice) =
struct
  include Shift.LexicalPair (X) (Y)

  let bot = pair X.bot Y.bot

  let join x y =
    let join_fst = X.join (fst x) (fst y) in
    let join_snd =
      match X.equal (fst x) join_fst, X.equal (fst y) join_fst with
      | false, false -> Y.bot
      | true, false -> snd x
      | false, true -> snd y
      | true, true -> Y.join (snd x) (snd y)
    in
    pair join_fst join_snd
end

module LiftToExpr (Var : OrderedType) (Base : Semilattice) :
sig
  include PartiallyOrderedType
  val var : Var.t -> t
  val subst : (Var.t -> t) -> t -> t
  val join : t -> t -> t
  val const : Base.t -> t
  val act : Base.t -> t -> t
end
=
struct
  module M = Map.Make (Var)

  type t = { const : Base.t; vars : Base.t M.t }

  let const s = { const = s; vars = M.empty }
  let var v = { const = Base.id; vars = M.singleton v Base.id }
  let act s e =
    { const = Base.compose s e.const
    ; vars = M.map (Base.compose s) e.vars
    }
  let join e1 e2 =
    { const = Base.join e1.const e2.const
    ; vars = M.union (fun _ x y -> Some (Base.join x y)) e1.vars e2.vars
    }
  let equal e1 e2 =
    Base.equal e1.const e2.const && M.equal Base.equal e1.vars e2.vars
  let lt e1 e2 =
    e1.const < e2.const &&
    M.for_all (fun v s1 -> match M.find_opt v e2.vars with Some s2 -> s1 < s2 | None -> false) e1.vars
  let leq e1 e2 =
    e1.const <= e2.const &&
    M.for_all (fun v s1 -> match M.find_opt v e2.vars with Some s2 -> s1 <= s2 | None -> false) e1.vars
  let subst f e =
    join (const e.const) @@
    M.fold (fun v s e -> join (act s (f v)) e) e.vars (const Base.id)
  let dump_vars fmt vs =
    Format.pp_print_seq
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@,")
      (fun fmt (v, s) ->
         if Base.is_id s then
           Format.fprintf fmt "@[<2>var(@,@[%a@])@]" Var.dump v
         else
           Format.fprintf fmt "@[<2>shift(@,@[<2>var(@[%a@])@],@,@[%a@])@]" Var.dump v Base.dump s)
      fmt
      (M.to_seq vs)
  let dump fmt e =
    if M.is_empty e.vars then
      Base.dump fmt e.const
    else if Base.is_id e.const then
      Format.fprintf fmt "@[<2>join(%a)@]" dump_vars e.vars
    else
      Format.fprintf fmt "@[<2>join(@,@[%a@],@,@[<2>join(@,@[%a@])@])@]" Base.dump e.const dump_vars e.vars
end

module Make (Var : OrderedType) (Base : Semilattice) :
sig
  include Shift.S

  module Expr :
  sig
    include PartiallyOrderedType
    val var : Var.t -> t
    val subst : (Var.t -> t) -> t -> t
    val join : t -> t -> t
    val const : Base.t -> t
    val act : Base.t -> t -> t
  end
  val join : t -> t -> t
  val of_seq : (Var.t * Expr.t) Seq.t -> t
  val to_seq : t -> (Var.t * Expr.t) Seq.t
end
=
struct
  module M = Map.Make (Var)
  module Expr = LiftToExpr (Var) (Base)

  type expr = Expr.t
  type t = expr M.t

  let of_seq s : t = M.of_seq s
  let to_seq (s : t) = M.to_seq s

  let id = M.empty

  let zip s1 s2 =
    M.merge
      (fun v e1 e2 ->
         let e1 = Option.value ~default:(Expr.var v) e1
         and e2 = Option.value ~default:(Expr.var v) e2
         in
         Some (e1, e2))
      s1 s2

  let equal s1 s2 =
    M.for_all (fun _ (e1, e2) -> Expr.equal e1 e2) (zip s1 s2)

  let is_id s = M.for_all (fun v e -> Expr.equal (Expr.var v) e) s

  let lt s1 s2 =
    M.for_all (fun _ (e1, e2) -> Expr.lt e1 e2) (zip s1 s2)

  let leq s1 s2 =
    let s1s2 = zip s1 s2 in
    M.for_all (fun _ (e1, e2) -> Expr.leq e1 e2) s1s2 &&
    M.exists (fun _ (e1, e2) -> Expr.lt e1 e2) s1s2

  let subst (s : t) (e : Expr.t) : Expr.t =
    Expr.subst (fun v -> Option.value ~default:(Expr.var v) (M.find_opt v s)) e

  let compose s1 s2 : t =
    M.map (fun (_, e2) -> subst s1 e2) (zip s1 s2)

  let join s1 s2 =
    M.map (fun (e1, e2) -> Expr.join e1 e2) (zip s1 s2)

  let dump fmt s =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_seq
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@,")
         (fun fmt (v, e) -> Format.fprintf fmt "@[.@[%a@]@,=%a@]" Var.dump v Expr.dump e))
      (M.to_seq s)
end