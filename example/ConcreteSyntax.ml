(** The concrete syntax of a minimal language with only variables and universes. *)
type t =
  | Var of int
  | Univ of t
  | TpULvl
  | Shift of t * int
