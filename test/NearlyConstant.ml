module N = Mugen.Shift.NearlyConstant (Mugen.Shift.Int)

let of_ints (base, prefix) =
  N.of_based_list (Mugen.Shift.Int.of_int base, List.map Mugen.Shift.Int.of_int prefix)

(* Expected comparisons are for the infinite sequences represented by each
   based list, including the constant tail after the finite prefix. *)
let cases = [
  "left tail prevents equality and ordering", (1, [0]), (0, []), (false, false, false);
  "right tail prevents equality and gives strict ordering", (0, []), (1, [0]), (false, true, true);
  "left tail gives strict ordering", (0, [1]), (1, []), (false, true, true);
  "right tail prevents ordering", (1, []), (0, [1]), (false, false, false);
  "strict inequality later in left prefix", (1, [1; 0]), (1, []), (false, true, true);
  "strict inequality later in right prefix", (0, []), (0, [0; 1]), (false, true, true);
  "equal sequences are not strictly ordered", (1, [0]), (1, [0]), (true, true, false);
  "crossing prefixes are incomparable", (0, [0; 1]), (0, [1]), (false, false, false);
]

let () =
  let failures = ref 0 in
  List.iter (fun (name, left, right, (equal, leq, lt)) ->
    let left = of_ints left and right = of_ints right in
    List.iter (fun (operation, actual, expected) ->
      if actual <> expected then begin
        incr failures;
        Printf.eprintf "%s: %s returned %b, expected %b\n" name operation actual expected
      end
    ) ["equal", N.equal left right, equal;
       "leq", N.leq left right, leq;
       "lt", N.lt left right, lt]
  ) cases;
  if !failures <> 0 then failwith (Printf.sprintf "%d comparison regressions" !failures)
