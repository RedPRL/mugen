open Bwd

type env = t bwd

(** The (NbE) domain. *)
and t =
  | Var of int
  | Univ of t
  | TpULvl
  | ULvl of (Syntax.ULvlShift.t, t) Mugen.Syntax.endo
  (** Use [endo] to embed universe levels into your datatype. *)

(** Instantiate the theory module to handle universe levels your datatype. *)
module ULvl =
  Mugen.Semantics.Endo.Make
    (struct
      module Shift = Syntax.ULvlShift
      type level = t
      let level l = ULvl l
      let unlevel = function ULvl l -> Some l | _ -> None
    end)
