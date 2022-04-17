type 'a endo = 'a * Shift.t

type 'v free =
  | Level of 'v free endo
  | Var of 'v

module Endo =
struct
  type 'a t = 'a endo

  let level b s = b, s

  let dump dump_a fmt (base, shift) =
    Format.fprintf fmt "@[<hv 1>level[@,@[%a@];@,@[%a@]]@]" dump_a base Shift.dump shift
end

module Free =
struct
  type 'v t = 'v free =
    | Level of 'v free endo
    | Var of 'v

  let level b s = Level (b, s)

  let var v = Var v

  let rec dump dump_v fmt =
    function
    | Level ulevel -> Endo.dump (dump dump_v) fmt ulevel
    | Var v -> dump_v fmt v
end
