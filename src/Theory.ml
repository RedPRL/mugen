include TheorySigs

module Make (P : Param) : S with type shift := P.Shift.t and type var := P.var =
struct
  open Syntax.Free

  include P
  include Builder.Free.Make(P)

  let normalize l =
    let rec go l acc =
      match l with
      | Level Top ->
        if acc = []
        then Level Top
        else invalid_arg "cannot shift the top level"
      | Level (Shifted (l, s)) -> go l (s :: acc)
      | Var v -> Level (Shifted (Var v, List.fold_left Shift.compose Shift.id acc))
    in
    go l []

  let equal x y =
    match normalize x, normalize y with
    | Level Top, Level Top -> true
    | Level (Shifted (Var vx, sx)), Level (Shifted (Var vy, sy)) ->
      equal_var vx vy && Shift.equal sx sy
    | _ -> false

  let lt x y =
    match normalize x, normalize y with
    | Level (Shifted (Var _, _)), Level Top -> true
    | Level (Shifted (Var vx, sx)), Level (Shifted (Var vy, sy)) ->
      equal_var vx vy && Shift.lt sx sy
    | _ -> false

  let leq x y =
    match normalize x, normalize y with
    | _, Level Top -> true
    | Level (Shifted (Var vx, sx)), Level (Shifted (Var vy, sy)) ->
      equal_var vx vy && Shift.leq sx sy
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
