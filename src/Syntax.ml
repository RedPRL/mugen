type ('s, 'a) endo =
  | Shifted of 'a * 's
  | Top

type ('s, 'v) free =
  | Level of ('s, ('s, 'v) free) endo
  | Var of 'v

module Endo =
struct
  type ('s, 'a) t = ('s, 'a) endo =
    | Shifted of 'a * 's
    | Top

  let shifted l s = Shifted (l, s)
  let top = Top

  let dump dump_s dump_a fmt =
    function
    | Shifted (base, shift) ->
      Format.fprintf fmt "@[<2>Shifted@ @[@[<1>(%a@],@, @[%a@])@]@]" dump_a base dump_s shift
    | Top ->
      Format.pp_print_string fmt "Top"
end

module Free =
struct
  type ('s, 'v) t = ('s, 'v) free =
    | Level of ('s, ('s, 'v) free) endo
    | Var of 'v

  let shifted l s = Level (Shifted (l, s))

  let top = Level Top

  let var v = Var v

  let rec dump dump_s dump_v fmt =
    function
    | Level ulevel -> Endo.dump dump_s (dump dump_s dump_v) fmt ulevel
    | Var v -> dump_v fmt v
end
