module Endo =
struct
  include TheorySigs.Endo

  module Make (P : Param) : S with type shift := P.Shift.t and type level := P.level =
  struct
    include P
    open Syntax.Endo

    let top = level Top

    let shifted l s =
      if Shift.is_id s then l
      else
        match unlevel l with
        | Some Top -> invalid_arg "cannot shift the top level"
        | Some (Shifted (l, s')) ->
          let s = Shift.compose s' s in
          level @@ Shifted (l, s)
        | None ->
          level @@ Shifted (l, s)
  end
end

module Free =
struct
  include TheorySigs.Free

  module Make (P : Param) : S with type shift := P.Shift.t and type var := P.var =
  struct
    open Syntax.Free

    let var = var
    module P = struct
      include P
      type level = (Shift.t, var) Syntax.free
      let level t = Level t
      let unlevel t = match t with Level l -> Some l | _ -> None
    end

    include P
    include Endo.Make(P)

    let dissect l =
      let rec go l acc =
        match l with
        | Level Top ->
          if Shift.is_id acc
          then level Top, acc
          else invalid_arg "cannot shift the top level"
        | Level (Shifted (l, s)) -> go l (Shift.compose s acc)
        | Var v -> Var v, acc
      in
      go l Shift.id

    let equal x y =
      match dissect x, dissect y with
      | (Level Top, _), (Level Top, _) -> true
      | (Var vx, sx), (Var vy, sy) ->
        equal_var vx vy && Shift.equal sx sy
      | _ -> false

    let lt x y =
      match dissect x, dissect y with
      | (Var _, _), (Level Top, _) -> true
      | (Var vx, sx), (Var vy, sy) ->
        equal_var vx vy && Shift.lt sx sy
      | _ -> false

    let leq x y =
      match dissect x, dissect y with
      | _, (Level Top, _) -> true
      | (Var vx, sx), (Var vy, sy) ->
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
end
