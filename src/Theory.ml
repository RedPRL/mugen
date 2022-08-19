module type Param = TheorySigs.Param
module type S = TheorySigs.S

module Make (P : Param) : S with type shift := P.Shift.t and type var := P.var =
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

  let leq x y =
    match dissect x, dissect y with
    | _, (Level Top, _) -> true
    | (Var vx, sx), (Var vy, sy) ->
      equal_var vx vy && Shift.leq (Option.value ~default:Shift.id sx) (Option.value ~default:Shift.id sy)
    | _ -> false

  let gt x y = lt y x
  let geq x y = leq y x

  module Infix =
  struct
    let (=) = equal
    let (<) = lt
    let (<=) = leq
    let (>) = gt
    let (>=) = geq
  end
end
