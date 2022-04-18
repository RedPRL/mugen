type ('s, 'a) endo =
  | Shifted of 'a * 's
  | Top

type ('s, 'v) free =
  | Level of ('s, ('s, 'v) free) endo
  | Var of 'v

module Endo :
sig
  type ('s, 'a) t = ('s, 'a) endo =
    | Shifted of 'a * 's
    | Top

  val shifted : 'a -> 's -> ('s, 'a) t
  val top : ('s, 'a) t
  val dump :
    (Format.formatter -> 's -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> ('s, 'a) t -> unit
end

module Free :
sig
  type ('s, 'v) t = ('s, 'v) free =
    | Level of ('s, ('s, 'v) free) endo
    | Var of 'v

  val shifted : ('s, 'v) t -> 's -> ('s, 'v) t
  val top : ('s, 'v) t
  val var : 'v -> ('s, 'v) t
  val dump :
    (Format.formatter -> 's -> unit) ->
    (Format.formatter -> 'v -> unit) ->
    Format.formatter -> ('s, 'v) t -> unit
end
