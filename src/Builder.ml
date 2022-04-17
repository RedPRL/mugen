module Endo =
struct
  module type Param =
  sig
    type level
    val level : level Syntax.endo -> level
    val unlevel : level -> level Syntax.endo option
  end

  module type S =
  sig
    type level
    val shift : Shift.t -> level -> level
    val simplify : level -> level
  end

  module Make (P : Param) : S with type level = P.level =
  struct
    include P

    let shift s l =
      match unlevel l with
      | Some (l, s') ->
        let s = Shift.compose s' s in
        if Shift.is_id s then l else level (l, s)
      | None -> level (l, s)

    let simplify l =
      let rec go l acc =
        match unlevel l with
        | Some (l, s) -> go l (s :: acc)
        | None -> l, acc
      in
      match go l [] with
      | l, [] -> l
      | l, s::ss -> level (l, List.fold_left Shift.compose s ss)
  end
end

module Free =
struct
  module type Param =
  sig
    type var
  end

  module type S =
  sig
    type var
    type level = var Syntax.free

    val var : var -> level
    include Endo.S with type level := level
  end

  module Make (P : Param) : S with type var = P.var =
  struct
    open Syntax.Free

    let var = var
    module P = struct
      include P
      type level = var Syntax.free
      let level t = Level t
      let unlevel t = match t with Level l -> Some l | _ -> None
    end

    include P
    include Endo.Make(P)
  end
end
