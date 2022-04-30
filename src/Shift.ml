open Bwd
open BwdNotation

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

module Gapped :
sig
  type t
  val id : t
  val const : int -> t
  val of_steps : int list -> t
  val of_skipped : int list -> t
  val of_prefix : int list -> t
  val is_id : t -> bool
  val equal : t -> t -> bool
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val compose : t -> t -> t
  val dump : Format.formatter -> t -> unit
end
=
struct
  type t = {init: int; steps: int list}

  let id = {init = 0; steps = []}

  let const init =
    if init < 0 then invalid_arg "Shift.Gapped.const";
    {init; steps = []}

  let drop_trailing_ones l =
    List.fold_right (fun x xs -> match x, xs with 1, [] -> [] | _ -> x :: xs) l []
  let of_steps =
    function
    | [] -> id
    | init :: steps ->
      if init < 0 || List.exists (fun i -> i < 1) steps then invalid_arg "Shift.Gapped.of_steps";
      {init; steps = drop_trailing_ones steps}

  let rec drop_trailing_ones_bwd =
    function Snoc (xs, 1) -> drop_trailing_ones_bwd xs | b -> BwdLabels.to_list b
  let rec increasing min = function [] -> true | x :: xs -> x >= min && increasing (x+1) xs
  let of_skipped skipped =
    if not (increasing 0 skipped) then invalid_arg "Shift.Gapped.of_skipped";
    let init, rest =
      let rec go cur =
        function
        | x :: xs when Int.equal x cur -> go (cur+1) xs
        | l -> cur, l
      in go 0 skipped
    in
    let steps =
      let rec go steps step cur =
        function
        | [] -> steps #< step
        | x :: xs when Int.equal x cur -> go steps (step+1) (cur+1) xs
        | l -> go (steps #< step) 1 (cur+1) l
      in
      go Emp 1 (init+1) rest
    in
    {init; steps = drop_trailing_ones_bwd steps}

  let of_prefix prefix =
    if not (increasing 0 prefix) then invalid_arg "Shift.Gapped.of_prefix";
    match prefix with
    | [] -> id
    | init :: _ ->
      let steps =
        let rec go steps =
          function
          | [] | [_] -> steps
          | [x; y] -> steps #< (y-x)
          | x :: y :: ys -> go (steps #< (y-x)) (y :: ys)
        in
        go Emp prefix
      in
      {init; steps = drop_trailing_ones_bwd steps}

  let is_id = function {init = 0; steps = []} -> true | _ -> false
  let equal s0 s1 =
    Int.equal s0.init s1.init &&
    List.equal Int.equal s0.steps s1.steps

  let rec lt s0 s1 =
    s0.init < s1.init &&
    match s0.steps, s1.steps with
    | [], _ -> true (* this is sufficient because a step >= 1 *)
    | _, [] ->
      let rec iter s i =
        match s.steps with
        | [] -> s.init < i
        | step :: steps -> iter {init = s.init + step; steps} (i + 1)
      in
      iter s0 s1.init
    | step0 :: steps0, step1 :: steps1 ->
      lt {init = s0.init + step0; steps = steps0} {init = s1.init + step1; steps = steps1}

  let rec le s0 s1 =
    s0.init <= s1.init &&
    match s0.steps, s1.steps with
    | [], _ -> true (* this is sufficient because a step >= 1 *)
    | _, [] ->
      let rec iter s i =
        match s.steps with
        | [] -> s.init <= i
        | step :: steps -> iter {init = s.init + step; steps} (i + 1)
      in
      iter s0 s1.init
    | step0 :: steps0, step1 :: steps1 ->
      le {init = s0.init + step0; steps = steps0} {init = s1.init + step1; steps = steps1}

  let rec compose_init s init1 =
    match s.steps, init1 with
    | _, 0 -> s
    | [], _ -> {init = s.init + init1; steps = []}
    | step :: steps, _ ->
      compose_init {init = s.init + step; steps} (init1-1)

  let rec compose_steps steps steps1 =
    match steps, steps1 with
    | _, [] -> steps
    | [], _ -> steps1
    | init :: steps, step1 :: steps1 ->
      let {init; steps} = compose_init {init; steps} step1 in
      init :: compose_steps steps steps1

  let compose s0 s1 =
    let {init; steps} = compose_init s0 s1.init in
    let steps = compose_steps steps s1.steps in
    {init; steps}

  let dump fmt s =
    Format.fprintf fmt "@[<hv 1>shift[@,%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,") Format.pp_print_int) (s.init :: s.steps)
end
type gapped = Gapped.t
