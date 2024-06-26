open Bwd

let rec eval_ulvl env =
  let module M = Mugen.Syntax in
  function
  | M.Top -> Domain.top
  | M.Shifted (b, s) -> Domain.shifted (eval env b) s

and eval env : Syntax.t -> Domain.t =
  function
  | Var i -> Bwd.nth env i
  | Univ l -> Univ (eval env l)
  | TpULvl -> TpULvl
  | ULvl l -> eval_ulvl env l

let rec quote_ulvl ctx : _ Mugen.Syntax.endo -> _ Mugen.Syntax.endo =
  function
  | Top -> Top
  | Shifted (b, s) -> Shifted (quote ctx b, s)

and quote ctx : Domain.t -> Syntax.t =
  function
  | Var i -> Var ((ctx-1) - i)
  | Univ l -> Univ (quote ctx l)
  | TpULvl -> TpULvl
  | ULvl l -> ULvl (quote_ulvl ctx l)

let equate_ulvl l1 l2 =
  assert (ULvl.equal (Domain.endo_to_ulvl l1) (Domain.endo_to_ulvl l2))

let rec equate ctx (v1 : Domain.t) (v2 : Domain.t) =
  match v1, v2 with
  | Var i1, Var i2 ->
    assert (Int.equal i1 i2)
  | Univ l1, Univ l2 ->
    equate ctx l1 l2
  | TpULvl, TpULvl ->
    ()
  | ULvl l1, ULvl l2 ->
    equate_ulvl l1 l2
  | _ ->
    failwith "equate"

let subtype _ctx (v1 : Domain.t) (v2 : Domain.t) =
  match v1, v2 with
  | Var i1, Var i2 ->
    assert (Int.equal i1 i2)
  | Univ l1, Univ l2 ->
    assert (ULvl.leq (Domain.to_ulvl l1) (Domain.to_ulvl l2))
  | TpULvl, TpULvl ->
    ()
  | ULvl l1, ULvl l2 ->
    equate_ulvl l1 l2
  | _ ->
    failwith "subtype"
