module type S =
sig
  type t
  val id : t
  val trans : int -> t
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
  let trans x = if x < 0 then invalid_arg "Shift.Crude.trans"; x
  let is_id = function 0 -> true | _ -> false
  let equal = Int.equal
  let lt : int -> int -> bool = (<)
  let le : int -> int -> bool = (<=)
  let compose : int -> int -> int = (+)
  let dump = Format.pp_print_int
end
type crude = Crude.t

module Linear :
sig
  include S
  val scale : int -> t
end
=
struct
  type t = {scale : int; trans : int} (* [f(i) = i * scale + trans] *)

  let (=) = Int.equal

  let id = {scale = 1; trans = 0}

  let trans trans =
    if trans < 0 then invalid_arg "Shift.Linear.translate";
    {scale = 1; trans}

  let scale scale =
    if scale < 1 then invalid_arg "Shift.Linear.scale";
    {scale; trans = 0}

  let is_id =
    function
    | {scale = 1; trans = 0} -> true
    | _ -> false

  let equal s0 s1 =
    Int.equal s0.scale s1.scale &&
    Int.equal s0.trans s1.trans

  let lt s0 s1 =
    s0.scale < s1.scale || (s0.scale = s1.scale && s0.trans < s1.trans)

  let le s0 s1 =
    s0.scale < s1.scale || (s0.scale = s1.scale && s0.trans <= s1.trans)

  let compose s0 s1 =
    {scale = s0.scale * s1.scale;
     trans = s0.scale * s1.trans + s0.trans}

  let dump fmt =
    function
    | {scale = 1; trans = 0} ->
      Format.fprintf fmt "(fun i -> i)"
    | {scale; trans = 0} ->
      Format.fprintf fmt "(fun i -> i * %i)" scale
    | {scale = 1; trans} ->
      Format.fprintf fmt "(fun i -> i + %i)" trans
    | {scale; trans} ->
      Format.fprintf fmt "(fun i -> i * %i + %i)" scale trans
end
type linear = Linear.t
