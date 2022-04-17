type 'a endo =
  | Shifted of 'a * Shift.t
  | Top

type 'v free =
  | Level of 'v free endo
  | Var of 'v

module Endo =
struct
  type 'a t = 'a endo =
    | Shifted of 'a * Shift.t
    | Top

  let shifted b s = Shifted (b, s)
  let top = Top

  let dump dump_a fmt =
    function
    | Shifted (base, shift) ->
      Format.fprintf fmt "@[<hv 1>shifted[@,@[%a@];@,@[%a@]]@]" dump_a base Shift.dump shift
    | Top ->
      Format.pp_print_string fmt "top"
end

module Free =
struct
  type 'v t = 'v free =
    | Level of 'v free endo
    | Var of 'v

  let shifted b s = Level (Shifted (b, s))

  let var v = Var v

  let rec dump dump_v fmt =
    function
    | Level ulevel -> Endo.dump (dump dump_v) fmt ulevel
    | Var v -> dump_v fmt v
end
