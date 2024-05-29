module ULvlShift = Mugen.Shift.Int
type ulvl_shift = ULvlShift.t
type ulvl = (ulvl_shift, t) Mugen.Syntax.endo

and t =
  | Var of int
  | Univ of t
  | TpULvl
  | ULvl of ulvl
