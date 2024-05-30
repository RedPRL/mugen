open Bwd

type env = t bwd

(** Use [endo] to embed universe levels into your datatype. *)
and ulvl = (ULvl.shift, t) Mugen.Syntax.endo

(** The (NbE) domain. *)
and t =
  | Var of int
  | Univ of t
  | TpULvl
  | ULvl of ulvl

(** Conversion from the domain to free universe level expressions *)
let rec to_ulvl : t -> ULvl.t =
  function
  | Var i -> Mugen.Syntax.Var i
  | ULvl endo -> endo_to_ulvl endo
  | _ -> invalid_arg "to_ulvl"

and endo_to_ulvl : ulvl -> ULvl.t =
  let module M = Mugen.Syntax in
  function
  | M.Shifted (l, s) -> ULvl.shifted (to_ulvl l) s
  | M.Top -> ULvl.top

(** Smart constructors for universe levels *)
include
  Mugen.Builder.Endo.Make
    (struct
      (** Your chosen displacement algebra *)
      module Shift = ULvl.Shift

      (** The type of embedded level expressions *)
      type level = t

      (** A function to embed a level expression *)
      let level (l : ulvl) : t = ULvl l

      (** A function to check whether an expression is an embedded level expression *)
      let unlevel : t -> ulvl option = function ULvl l -> Some l | _ -> None
    end)
