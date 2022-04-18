# 🌌 Universe Levels

An overly functorized implementation of [Conor McBride’s crude but effective stratification](https://personal.cis.strath.ac.uk/conor.mcbride/Crude.pdf) and its slight generalization.

## Stability

⚠ The API is experimental and unstable. We will break things!

## Components

- [Mugenjou.Shift](https://redprl.org/mugenjou/mugenjou/Mugenjou/Shift): classes of level shifting operators
- [Mugenjou.Syntax](https://redprl.org/mugenjou/mugenjou/Mugenjou/Syntax): universe levels
- [Mugenjou.Builder](https://redprl.org/mugenjou/mugenjou/Mugenjou/Builder): smart builders of universe levels
- [Mugenjou.Theory](https://redprl.org/mugenjou/mugenjou/Mugenjou/Theory): comparators for universe levels

## Philosophy and Beliefs in this Experiment

1. The distinguished level variable should be made explicit so that everything has clear semantics.
2. One-variable universe polymorphism with cumulativity is enough. Type ambiguity (as in Coq) and multi-variable universe polymorphism (as in Agda) are overkill.
3. It is convenient to have the top level.

## Classes of Level Shifting Operators

In Conor McBride’s notes, it was noted that any class of strictly monotone operators on levels closed under identity and composition will work. Initially, the class of functions `(fun i -> i + n)` for any `n >= 0` was chosen as it is practically the minimum usable class. We are additionally considering a larger class of strictly monotone functions that need to agree with `(fun i -> i + n)` only for large enough `i`. Both classes are implemented in this package. We are eager to experiment with any class that arises from practical needs.

- [Mugenjou.Shift.Crude](https://redprl.org/mugenjou/mugenjou/Mugenjou/Shift/Crude): the class of functions `(fun i -> i + n)`
- [Mugenjou.Shift.FinSkip](https://redprl.org/mugenjou/mugenjou/Mugenjou/Shift/FinSkip): the class of monotone functions that agree with `(fun i -> i + n)` for large enough `i`

## How to Use It

### OCaml >= 4.12

You need OCaml 4.12 or later

### Example Code

```ocaml
module M = Mugenjou.Syntax
type ulevel = (M.Shift.crude, int) M.free
let l : ulevel = M.(
```

### Documentation

[Here is the API documentation.](https://redprl.org/mugenjou/mugenjou/Mugenjou)
