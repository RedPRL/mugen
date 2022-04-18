module Endo =
struct
  module type Param =
  sig
    module Shift : Shift.S
    type level
    val level : (Shift.t, level) Syntax.endo -> level
    val unlevel : level -> (Shift.t, level) Syntax.endo option
  end

  module type S =
  sig
    module Shift : Shift.S
    type level
    val shifted : level -> Shift.t -> level
    val top : level
    val simplify : level -> level
    val dissect : level -> level * Shift.t option
  end

  module Make (P : Param) : S with module Shift := P.Shift and type level = P.level =
  struct
    include P
    open Syntax.Endo

    let shifted l s =
      if Shift.is_id s then l
      else
        match unlevel l with
        | Some Top -> level Top
        | Some (Shifted (l, s')) ->
          let s = Shift.compose s' s in
          level @@ Shifted (l, s)
        | None ->
          level @@ Shifted (l, s)

    let top = level Top

    let reduce_shifts =
      function
      | [] -> None
      | s::ss -> Some (List.fold_left Shift.compose s ss)

    let dissect l =
      let rec go l acc =
        match unlevel l with
        | Some Top -> level Top, None
        | Some (Shifted (l, s)) -> go l (s :: acc)
        | None -> l, reduce_shifts acc
      in
      go l []

    let simplify l =
      let l, ss = dissect l in
      match ss with
      | None -> l
      | Some s when Shift.is_id s -> l
      | Some s -> level @@ Shifted (l, s)
  end
end

module Free =
struct
  module type Param =
  sig
    module Shift : Shift.S
    type var
  end

  module type S =
  sig
    module Shift : Shift.S
    type var
    type level = (Shift.t, var) Syntax.free

    val var : var -> level
    include Endo.S with module Shift := Shift and type level := level
  end

  module Make (P : Param) : S  with module Shift := P.Shift and type var = P.var =
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
  end
end
