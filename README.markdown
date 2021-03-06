# ♾️ Mugen 無限: Universe Levels

A vast generalization of [Conor McBride’s crude but effective stratification](https://personal.cis.strath.ac.uk/conor.mcbride/Crude.pdf). We replace natural numbers with what we call _displacement algebras_, the minimum algebraic structure to use McBride’s scheme.

## Stability

⚠ The API is experimental and unstable. We will break things!

## Components

- [Mugen.Shift](https://redprl.org/mugen/mugen/Mugen/Shift): examples of displacement algebras
- [Mugen.ShiftWithJoin](https://redprl.org/mugen/mugen/Mugen/ShiftWithJoin): more examples of displacement algebras with joins
- [Mugen.Syntax](https://redprl.org/mugen/mugen/Mugen/Syntax): syntax of universe levels
- [Mugen.Builder](https://redprl.org/mugen/mugen/Mugen/Builder): smart builders of universe levels
- [Mugen.Theory](https://redprl.org/mugen/mugen/Mugen/Theory): comparators for universe levels

## Philosophy and Beliefs in this Experiment

1. The distinguished level variable for top-level definitions should be explicit in the core language for clean semantics. (It can remain implicit in the surface language.)
2. One-variable universe polymorphism with cumulativity is enough. Typical ambiguity (as in Coq) and multi-variable universe polymorphism (as in Agda) are overkill.
3. It is convenient to have the top level for type checking.

## Displacement Algebras

In Conor McBride’s notes, it was noted that any class of strictly monotone operators on levels closed under identity and composition will work. We codified such a class as a displacement algebra. The classic displacement operators may be recovered by using the following module with non-negative numbers:

- [Mugen.Shift.Int](https://redprl.org/mugen/mugen/Mugen/Shift/Int).

## How to Use It

### OCaml >= 4.13

You need OCaml 4.13 or later

### Example Code

```ocaml
module I = Mugen.Shift.Int
module M = Mugen.Syntax

(* The type of universe levels, using integers as displacements and strings as variable names. *)
type ulevel = (I.t, string) M.free

(* The level representing "x + 10" *)
let l : ulevel = M.Free.(shifted (var "x") (I.of_int 10))
```

### Documentation

[Here is the API documentation.](https://redprl.org/mugen/mugen/Mugen)
