open Bwd

type env = t bwd

and ulvl = (Syntax.ulvl_shift, t) Mugen.Syntax.endo

(** The (NbE) domain. *)
and t =
  | Var of int
  | Univ of t
  | TpULvl
  | ULvl of ulvl
  (** Use [endo] to embed universe levels into your datatype. *)

(** Include the builders. *)
module ULvl =
  Mugen.Builder.Endo.Make
    (struct
      module Shift = Syntax.ULvlShift
      type level = t
      let level (l : ulvl) : t = ULvl l
      let unlevel : t -> ulvl option = function ULvl l -> Some l | _ -> None
    end)
