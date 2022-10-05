type t =
  | Var of int
  | Univ of t
  | TpULvl
  | Shift of t * int
