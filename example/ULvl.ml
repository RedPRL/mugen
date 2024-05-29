(** A convenience module for freely generated universe level expressions. *)

type t = (Syntax.ulvl_shift, int) Mugen.Syntax.free

include
  Mugen.Builder.Free.Make
    (struct
      module Shift = Syntax.ULvlShift
      type var = int
    end)

include
  Mugen.Theory.Make
    (struct
      module Shift = Syntax.ULvlShift
      type var = int
      let equal_var = Int.equal
    end)

(** Conversion from the domain. *)
let rec of_con : Domain.t -> t =
  function
  | Domain.Var i -> Mugen.Syntax.Var i
  | Domain.ULvl endo -> of_endo endo
  | _ -> invalid_arg "of_con"

and of_endo : Domain.ulvl -> t =
  let module M = Mugen.Syntax in
  function
  | M.Shifted (l, s) -> shifted (of_con l) s
  | M.Top -> top
