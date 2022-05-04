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

module Trans : S =
struct
  type t = int
  let id = 0
  let trans x = if x < 0 then invalid_arg "Shift.Trans.trans"; x
  let is_id = function 0 -> true | _ -> false
  let equal = Int.equal
  let lt : int -> int -> bool = (<)
  let le : int -> int -> bool = (<=)
  let compose : int -> int -> int = (+)
  let dump = Format.pp_print_int
end
type trans = Trans.t

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
    s0.scale = s1.scale &&
    s0.trans = s1.trans

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

module LinearPostInc :
sig
  include S
  val scale : int -> t
  val postinc : int -> t
end
=
struct
  type t = {scale : int; trans : int; postinc : int}
  (* [g(f) = (fun i -> i + postinc) . f . (fun i -> i * scale + trans)] *)

  let id = {scale = 1; trans = 0; postinc = 0}

  let trans trans =
    if trans < 0 then invalid_arg "Shift.LinearPostInc.translate";
    {scale = 1; trans; postinc = 0}

  let scale scale =
    if scale < 1 then invalid_arg "Shift.LinearPostInc.scale";
    {scale; trans = 0; postinc = 0}

  let postinc postinc =
    if postinc < 0 then invalid_arg "Shift.LinearPostInc.postinc";
    {scale = 1; trans = 0; postinc}

  let is_id =
    function
    | {scale = 1; trans = 0; postinc = 0} -> true
    | _ -> false

  let equal s0 s1 =
    s0.scale = s1.scale &&
    s0.trans = s1.trans &&
    s0.postinc = s1.postinc

  let lt s0 s1 =
    s0.scale < s1.scale
    || (s0.scale = s1.scale && s0.trans < s1.trans)
    || (s0.scale = s1.scale && s0.trans = s1.trans && s0.postinc < s1.postinc)

  let le s0 s1 =
    s0.scale < s1.scale
    || (s0.scale = s1.scale && s0.trans < s1.trans)
    || (s0.scale = s1.scale && s0.trans = s1.trans && s0.postinc <= s1.postinc)

  let compose s0 s1 =
    {scale = s0.scale * s1.scale;
     trans = s0.scale * s1.trans + s0.trans;
     postinc = s0.postinc + s1.postinc}

  let dump fmt =
    function
    | {scale = 1; trans = 0; postinc = 0} ->
      Format.fprintf fmt "(fun f i -> f i)"
    | {scale = 1; trans = 0; postinc} ->
      Format.fprintf fmt "(fun f i -> f i + %i)" postinc
    | {scale; trans = 0; postinc = 0} ->
      Format.fprintf fmt "(fun f i -> f (i * %i))" scale
    | {scale; trans = 0; postinc} ->
      Format.fprintf fmt "(fun f i -> f (i * %i) + %i)" scale postinc
    | {scale = 1; trans; postinc = 0} ->
      Format.fprintf fmt "(fun f i -> f (i + %i))" trans
    | {scale = 1; trans; postinc} ->
      Format.fprintf fmt "(fun f i -> f (i + %i) + %i)" trans postinc
    | {scale; trans; postinc = 0} ->
      Format.fprintf fmt "(fun f i -> f (i * %i + %i))" scale trans
    | {scale; trans; postinc} ->
      Format.fprintf fmt "(fun f i -> f (i * %i + %i) + %i)" scale trans postinc
end
type lpi = LinearPostInc.t

module DualTrans :
sig
  include S
  val secondary_trans : int -> t
end
=
struct
  type t = {trans : int; secondary_trans : int}

  let id = {trans = 0; secondary_trans = 0}

  let trans trans =
    if trans < 0 then invalid_arg "Shift.LinearPostInc.translate";
    {trans; secondary_trans = 0}

  let secondary_trans secondary_trans =
    if secondary_trans < 1 then invalid_arg "Shift.LinearPostInc.secondary_trans";
    {trans = 0; secondary_trans}

  let is_id =
    function
    | {trans = 0; secondary_trans = 0} -> true
    | _ -> false

  let equal s0 s1 =
    s0.trans = s1.trans &&
    s0.secondary_trans = s1.secondary_trans

  let lt s0 s1 =
    s0.trans < s1.trans
    || (s0.trans = s1.trans && s0.secondary_trans < s1.secondary_trans)

  let le s0 s1 =
    s0.trans < s1.trans
    || (s0.trans = s1.trans && s0.secondary_trans <= s1.secondary_trans)

  let compose s0 s1 =
    {trans = s1.trans + s0.trans;
     secondary_trans = s0.secondary_trans + s1.secondary_trans}

  let dump fmt =
    function
    | {trans = 0; secondary_trans = 0} ->
      Format.fprintf fmt "(fun (x, y) -> (x, y))"
    | {trans; secondary_trans = 0} ->
      Format.fprintf fmt "(fun (x, y) -> (x + %i, y))" trans
    | {trans = 0; secondary_trans} ->
      Format.fprintf fmt "(fun (x, y) -> (x, y + %i))" secondary_trans
    | {trans; secondary_trans} ->
      Format.fprintf fmt "(fun (x, y) -> (x + %i, y + %i))" trans secondary_trans
end
type dual_trans = DualTrans.t
