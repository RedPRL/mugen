module Endo =
struct
  include SemanticsSigs.Endo

  module Make (P : Param) : S with type shift := P.Shift.t and type level := P.level =
  struct
    include P
    open Syntax.Endo

    let top = level Top

    let shifted l s =
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
  include SemanticsSigs.Free

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

    let normalize l =
      let rec go l acc =
        match l with
        | Level Top ->
          if acc = []
          then level Top
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
end
