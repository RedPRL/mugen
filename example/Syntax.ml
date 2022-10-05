module ULvlShift = Mugen.Shift.Int

type t =
  | Var of int
  | Univ of t
  | TpULvl
  | ULvl of (ULvlShift.t, t) Mugen.Syntax.endo
