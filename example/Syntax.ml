type ulvl = (ULvl.shift, t) Mugen.Syntax.endo

and t =
  | Var of int
  | Univ of t
  | TpULvl
  | ULvl of ulvl
