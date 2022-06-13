open StructuredType

module type BoundedSemilattice =
sig
  include Shift.S
  val bot : t
  val join : t -> t -> t
end

module Nat =
struct
  type t = Shift.Int.t
  let of_int x = if x < 0 then invalid_arg "BoundedSemilattice.Nat.of_int"; Shift.Int.of_int x
  let bot = of_int 0
  let id = Shift.Int.id
  let to_int = Shift.Int.to_int
  let equal = Shift.Int.equal
  let is_id = Shift.Int.is_id
  let lt = Shift.Int.lt
  let leq = Shift.Int.leq
  let compose = Shift.Int.compose
  let join x y = of_int (Int.max (to_int x) (to_int y))
  let dump = Shift.Int.dump
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

module type Expr =
sig
end

module LiftToExpr (Var : OrderedType) (Base : BoundedSemilattice) :
sig
  include PartiallyOrderedType
  val var : Var.t -> t
  val subst : (Var.t -> t) -> t -> t
  val bot : t
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
  let bot = const Base.bot
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
    max (const e.const) @@
    M.fold (fun v s e -> max (act s (f v)) e) e.vars (const Base.id)
  let dump_vars fmt vs =
    Format.pp_print_seq
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@,")
      (fun fmt (v, s) ->
         if Base.is_id s then
           Format.fprintf fmt ".@[%a@]" Var.dump v
         else
           Format.fprintf fmt "@[<2>(.@[%a@],@,@[%a@])@]" Var.dump v Base.dump s)
      fmt
      (M.to_seq vs)
  let dump fmt e =
    if M.is_empty e.vars then
      Base.dump fmt e.const
    else if Base.is_id e.const then
      Format.fprintf fmt "@[<2>max(%a)@]" dump_vars e.vars
    else
      Format.fprintf fmt "@[<2>(max(@,@[%a@]),@,@[%a@])@]" dump_vars e.vars Base.dump e.const
end

module Make (Var : OrderedType) (Base : BoundedSemilattice) :
sig
  include Shift.S
  val join : t -> t -> t
  val of_seq : (Var.t * LiftToExpr(Var)(Base).t) Seq.t -> t
  val to_seq : t -> (Var.t * LiftToExpr(Var)(Base).t) Seq.t
end
=
struct
  module M = Map.Make (Var)
  module Expr = LiftToExpr (Var) (Base)

  type expr = Expr.t
  type t = expr M.t

  let find v s = Option.value ~default:(Expr.var v) (M.find_opt v s)
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

  let is_id s = M.for_all (fun k e -> Expr.equal (Expr.var k) e) s

  let lt s1 s2 =
    M.for_all (fun _ (e1, e2) -> Expr.lt e1 e2) (zip s1 s2)

  let leq s1 s2 =
    let s1s2 = zip s1 s2 in
    M.for_all (fun _ (e1, e2) -> Expr.leq e1 e2) s1s2 &&
    M.exists (fun _ (e1, e2) -> Expr.lt e1 e2) s1s2

  let subst s e = Expr.subst (fun v -> find v s) e

  let compose s1 s2 =
    M.merge (fun _ e1 ->
        function
        | None -> e1
        | Some e2 -> Some (subst s1 e2))
      s1 s2

  let join s1 s2 =
    M.map (fun (e1, e2) -> Expr.join e1 e2) @@ zip s1 s2

  let dump fmt s =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_seq
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@,")
         (fun fmt (v, e) -> Format.fprintf fmt "@[.@[%a@]@,=%a@]" Var.dump v Expr.dump e))
      (M.to_seq s)
end
