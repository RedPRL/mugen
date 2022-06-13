# ♾️ Mugen 無限: Universe Levels

An overly functorized implementation of [Conor McBride’s trans but effective stratification](https://personal.cis.strath.ac.uk/conor.mcbride/Trans.pdf) and its slight generalization.

## Stability

⚠ The API is experimental and unstable. We will break things!

## Components

- [Mugen.Shift](https://redprl.org/mugen/mugen/Mugen/Shift): classes of level shifting operators
- [Mugen.Syntax](https://redprl.org/mugen/mugen/Mugen/Syntax): universe levels
- [Mugen.Builder](https://redprl.org/mugen/mugen/Mugen/Builder): smart builders of universe levels
- [Mugen.Theory](https://redprl.org/mugen/mugen/Mugen/Theory): comparators for universe levels

## Philosophy and Beliefs in this Experiment

1. The distinguished level variable should be made explicit so that everything has clear semantics.
2. One-variable universe polymorphism with cumulativity is enough. Typical ambiguity (as in Coq) and multi-variable universe polymorphism (as in Agda) are overkill.
3. It is convenient to have the top level.

## Classes of Level Shifting Operators

In Conor McBride’s notes, it was noted that any class of strictly monotone operators on levels closed under identity and composition will work. Initially, the class of functions `f(i) = i + n` for `n >= 0` was chosen as it is practically the minimum usable class. This is implemented in this module:

- [Mugen.Shift.Int](https://redprl.org/mugen/mugen/Mugen/Shift/Int): the class of functions `f(i) = i + n`.

We are experimenting with many other classes.

## How to Use It

### OCaml >= 4.13

You need OCaml 4.13 or later

### Example Code

```ocaml
module MS = Mugen.Shift
module M = Mugen.Syntax

type ulevel = (MS.trans, string) M.free

let l : ulevel = M.Free.(shifted (var "x") (MS.Trans.trans 10))
```

### Documentation

[Here is the API documentation.](https://redprl.org/mugen/mugen/Mugen)
