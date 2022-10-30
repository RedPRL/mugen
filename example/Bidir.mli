type cell = {tm : Domain.t; tp : Domain.t}
type ctx = cell Bwd.bwd

val check : ctx -> ConcreteSyntax.t -> Domain.t -> Syntax.t
val infer : ctx -> ConcreteSyntax.t -> Syntax.t * Domain.t
