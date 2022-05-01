module type S =
sig
  type t
  val id : t
  val const : int -> t
  val is_id : t -> bool
  val equal : t -> t -> bool
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val compose : t -> t -> t
  val dump : Format.formatter -> t -> unit
end

module Crude : S =
struct
  type t = int
  let id = 0
  let const x = if x < 0 then invalid_arg "Shift.Crude.const"; x
  let is_id = function 0 -> true | _ -> false
  let equal = Int.equal
  let lt : int -> int -> bool = (<)
  let le : int -> int -> bool = (<=)
  let compose : int -> int -> int = (+)
  let dump = Format.pp_print_int
end
type crude = Crude.t
