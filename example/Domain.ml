open Bwd

type env = t bwd

and t =
  | Var of int
  | Univ of t
  | TpULvl
  | ULvl of (Syntax.ULvlShift.t, t) Mugen.Syntax.endo

module ULvl =
  Mugen.Theory.Endo.Make
    (struct
      module Shift = Syntax.ULvlShift
      type level = t
      let level l = ULvl l
      let unlevel = function ULvl l -> Some l | _ -> None
    end)
