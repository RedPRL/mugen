(** Structured types used in this library *)
module StructuredType : module type of StructuredType

(** Displacement algebras *)
module Shift : module type of Shift

(** Displacement algebras with joins *)
module ShiftWithJoin : module type of ShiftWithJoin

(** Syntax of universe levels with displacements *)
module Syntax : module type of Syntax

(** Semantic operations for universe levels with displacements *)
module Semantics : module type of Semantics
