type t = {init: int; steps: int list}

let id = {init = 0; steps = []}

let const init =
  if init < 0 then
    invalid_arg "Shift.const"
  else
    {init; steps = []}

let rec drop_ending_ones =
  function
  | [] -> []
  | 1::xs ->
    begin
      match drop_ending_ones xs with
      | [] -> []
      | xs -> 1 :: xs
    end
  | x::xs -> x :: drop_ending_ones xs

let make ~init ~steps =
  if init < 0 || List.exists (fun i -> i < 1) steps then
    invalid_arg "Shift.make"
  else
    {init; steps = drop_ending_ones steps}

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
