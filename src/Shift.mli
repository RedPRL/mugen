module type S =
sig
  type t
  val id : t
  val const : int -> t
  val is_id : t -> bool
  val equal : t -> t -> bool
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val compose : t -> t -> t
  val dump : Format.formatter -> t -> unit
end

(** Conor McBride's crude universe levels. *)
module Fixed : S

(** Generalized {!module:Fixed} that allows finite gaps. *)
module FinGap :
sig
  include S
  val make : init:int -> steps:int list -> t
end
