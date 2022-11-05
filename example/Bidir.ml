open Bwd

module CS = ConcreteSyntax

type cell = {tm : Domain.t; tp : Domain.t}
type ctx = cell bwd

let to_env ctx = Bwd.map (fun {tm; _} -> tm) ctx
let to_size ctx = Bwd.length ctx

let shift s = Syntax.ULvlShift.of_int s

(** Type checking. *)
let rec check ctx (tm : CS.t) (tp : Domain.t) =
  match tm, tp with
  | CS.Univ l1, Univ l2 ->
    let l1 = check ctx l1 TpULvl in
    assert (ULvl.lt (ULvl.of_con (NbE.eval (to_env ctx) l1)) (ULvl.of_con l2));
    Univ l1
  | CS.TpULvl, Univ (ULvl Top) -> TpULvl
  | CS.Shift (l, s), TpULvl ->
    let l = check ctx l TpULvl in
    ULvl (Shifted (l, shift s))
  | _ ->
    let tm, tp' = infer ctx tm in
    NbE.subtype (to_size ctx) tp' tp;
    tm

(** Type inference. *)
and infer ctx =
  function
  | CS.Var i ->
    let {tm; tp} = Bwd.nth ctx i in
    NbE.quote (to_size ctx) tm, tp
  | _ -> failwith "not inferable"
