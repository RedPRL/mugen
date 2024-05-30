(** {1 Gallery of Displacement Algebras} *)

(** Stock displacement algebras *)
module Shift : module type of Shift

(** Stock displacement algebras with joins *)
module ShiftWithJoin : module type of ShiftWithJoin

(** {1 Syntax of Level Expressions} *)

(** Syntax of universe level expressions *)
module Syntax : module type of Syntax

(** Smart builders for universe level expressions *)
module Builder : module type of Builder

(** {1 Comparators of Level Expressions} *)

(** Semantic operations for universe levels with displacements *)
module Theory : module type of Theory

(**/**)

(** Structured types used in this library *)
module StructuredType : module type of StructuredType
