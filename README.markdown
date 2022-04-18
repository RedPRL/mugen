# 🌌 Universe Levels

An overly functorized implementation of [Conor McBride’s crude but effective stratification](https://personal.cis.strath.ac.uk/conor.mcbride/Crude.pdf) and its slight generalization.

## Philosophy and Beliefs in this Experiment

1. The one level variable should be made explicit so that everything has clear semantics.
2. One-variable universe polymorphism with cumulativity is enough. Type ambiguity (as in Coq) and multi-variable universe polymorphism (as in Agda) are overkill.

## Classes of Level Shifting Operators

It was noted in the Conor McBride’s notes that any class of strictly monotone operators on levels closed under identity and composition will work. Originally, the class of functions `(fun i -> i + n)` for any `n >= 0` was chosen as it is practically the minimum usable class. We are also considering a larger class of strictly monotone functions that agree with `(fun i -> i + n)` only for large enough `i`. Both classes are implemented in this package.

- `Mugenjou.Shift.Fixed`: the class of functions `(fun i -> i + n)`
- `Mugenjou.Shift.FinSkip`: the class of monotone functions that agree with `(fun i -> i + n)` for large enough `i`
