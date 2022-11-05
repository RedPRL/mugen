type cell = {tm : Domain.t; tp : Domain.t}
(** Cells in a typed context *)

type ctx = cell Bwd.bwd
(** Contexts for type checking *)

val check : ctx -> ConcreteSyntax.t -> Domain.t -> Syntax.t
(** [check ctx tm tp] checks the term [tm] against the type [tp] and returns the elaborated syntax. *)

val infer : ctx -> ConcreteSyntax.t -> Syntax.t * Domain.t
(** [infer ctx tm] takes a term [tm] and returns its elaborated syntax and inferred type. *)
