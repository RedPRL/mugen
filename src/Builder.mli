module Endo :
sig
  module type Param =
  sig
    module Shift : Shift.S
    type level
    val level : (Shift.t, level) Syntax.endo -> level
    val unlevel : level -> (Shift.t, level) Syntax.endo option
  end

  module type S =
  sig
    module Shift : Shift.S
    type level
    val shifted : level -> Shift.t -> level
    val top : level
    val dissect : level -> level * Shift.t option
    val simplify : level -> level
  end

  module Make (P : Param) : S with module Shift := P.Shift and type level = P.level
end

module Free :
sig
  module type Param =
  sig
    module Shift : Shift.S
    type var
  end

  module type S =
  sig
    module Shift : Shift.S
    type var
    type level = (Shift.t, var) Syntax.free

    val var : var -> level
    include Endo.S with module Shift := Shift and type level := level
  end

  module Make (P : Param) : S  with module Shift := P.Shift and type var = P.var
end
