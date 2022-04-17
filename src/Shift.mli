type t
val id : t
val const : int -> t
val make : init:int -> steps:int list -> t
val is_id : t -> bool
val equal : t -> t -> bool
val lt : t -> t -> bool
val le : t -> t -> bool
val compose : t -> t -> t
val dump : Format.formatter -> t -> unit
