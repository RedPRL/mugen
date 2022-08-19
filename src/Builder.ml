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
    type shift
    type level
    val shifted : level -> shift -> level
    val top : level
    val simplify : level -> level
    val dissect : level -> level * shift option
  end

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

    let reduce_shifts =
      function
      | [] -> None
      | s::ss ->
        let s = List.fold_left Shift.compose s ss in
        if Shift.is_id s then None else Some s

    let dissect l =
      let rec go l acc =
        match unlevel l with
        | Some Top ->
          if reduce_shifts acc = None
          then level Top, None
          else invalid_arg "cannot shift the top level"
        | Some (Shifted (l, s)) -> go l (s :: acc)
        | None -> l, reduce_shifts acc
      in
      go l []

    let simplify l =
      let l, ss = dissect l in
      match ss with
      | None -> l
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
    type shift
    type var
    type level = (shift, var) Syntax.free

    val var : var -> level
    include Endo.S with type shift := shift and type level := level
  end

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
  end
end
