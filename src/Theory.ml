module type Param =
sig
  module Shift : Shift.S
  type var
  val equal_var : var -> var -> bool
end

module type S =
sig
  module Shift : Shift.S
  type var
  type level = (Shift.t, var) Syntax.free
  val equal : level -> level -> bool
  val lt : level -> level -> bool
  val le : level -> level -> bool
  val gt : level -> level -> bool
  val ge : level -> level -> bool
  val (=) : level -> level -> bool
  val (<) : level -> level -> bool
  val (<=) : level -> level -> bool
  val (>) : level -> level -> bool
  val (>=) : level -> level -> bool
end

module Make (P : Param) : S with module Shift = P.Shift and type var = P.var =
struct
  open Syntax.Free
  include P
  include Builder.Free.Make(P)

  let equal x y =
    match dissect x, dissect y with
    | (Level Top, _), (Level Top, _) -> true
    | (Var vx, sx), (Var vy, sy) ->
      equal_var vx vy && Shift.equal (Option.value ~default:Shift.id sx) (Option.value ~default:Shift.id sy)
    | _ -> false

  let lt x y =
    match dissect x, dissect y with
    | (Var _, _), (Level Top, _) -> true
    | (Var vx, sx), (Var vy, sy) ->
      equal_var vx vy && Shift.lt (Option.value ~default:Shift.id sx) (Option.value ~default:Shift.id sy)
    | _ -> false

  let le x y =
    match dissect x, dissect y with
    | _, (Level Top, _) -> true
    | (Var vx, sx), (Var vy, sy) ->
      equal_var vx vy && Shift.le (Option.value ~default:Shift.id sx) (Option.value ~default:Shift.id sy)
    | _ -> false

  let gt x y = lt y x
  let ge x y = le y x

  let (=) = equal
  let (<) = lt
  let (<=) = le
  let (>) = gt
  let (>=) = ge
end
