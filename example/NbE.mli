val eval : Domain.env -> Syntax.t -> Domain.t
val quote : int -> Domain.t -> Syntax.t
val equate : int -> Domain.t -> Domain.t -> unit
val subtype : int -> Domain.t -> Domain.t -> unit
