# Chapter 1

## What is lambda?

Lambda is a function. A **function** is a relationship between a set of possible inputs and a set of possible outputs.

### What is lambda made of?
Lambda consists of:
  * head - `λ` symbol
  * arguments
  * body that the body is applied to

### Alpha equivalence

For example `𝜆𝑥.𝑥`. Is a form of equivalence between arguments and the body.

### Beta reduction

It's an application of arguments to the body of lambda. The arguments that are applied to the function are listed to its right eg. `(𝜆𝑥.𝑥) 2`

### Free variables

Variables that are not argument of the function but exist in the body eg. 𝜆𝑥.𝑥𝑦

### Multiple parameters

Each lambda can only accept one parameter therefore we can apply Curring eg.

```
𝜆𝑥𝑦.𝑥𝑦
𝜆𝑥.(𝜆𝑦.𝑥𝑦)
```

### Combinator

Combinator is a lambda with no free variables

### Divergence

Divergence is a form of recursion when one argument can be applied ad infinitum eg.

application of `(𝜆𝑥.𝑥𝑥)` to `(𝜆𝑥.𝑥𝑥)`
results in `(𝜆𝑥.𝑥𝑥)`
