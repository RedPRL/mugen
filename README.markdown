# 🌌 Universe Levels

An overly functorized implementation of [Conor McBride’s crude but effective stratification](https://personal.cis.strath.ac.uk/conor.mcbride/Crude.pdf) and its slight generalization.

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

In Conor McBride’s notes, it was noted that any class of strictly monotone operators on levels closed under identity and composition will work. Initially, the class of functions `f(i) = i + n` for `n >= 0` was chosen as it is practically the minimum usable class. We are additionally considering a larger class of strictly monotone functions `f` such that `f(i) = i + n` when `i >= k` for some finite `k` (when `i < k`, the function `f` is still strictly monotone, but may differ from `f’(i) = i + n`). Both classes are implemented in this package. We are eager to experiment with any class that arises from practical needs.

- [Mugen.Shift.Crude](https://redprl.org/mugen/mugen/Mugen/Shift/Crude): the class of functions `f(i) = i + n`.
- [Mugen.Shift.Gapped](https://redprl.org/mugen/mugen/Mugen/Shift/Gapped): the class of monotone functions that agree with `f(i) = i + n` for `i > k`.

## How to Use It

### OCaml >= 4.12

You need OCaml 4.12 or later

### Example Code

```ocaml
module MS = Mugen.Shift
module M = Mugen.Syntax

type ulevel = (MS.crude, string) M.free

let l : ulevel = M.Free.(shifted (var "x") (MS.Crude.const 10))
```

### Documentation

[Here is the API documentation.](https://redprl.org/mugen/mugen/Mugen)
