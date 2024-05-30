module Endo =
struct
  include BuilderSigs.Endo

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
  include BuilderSigs.Free

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
