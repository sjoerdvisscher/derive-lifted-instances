derive-lifted-instances
=======================

[![Hackage](https://img.shields.io/hackage/v/derive-lifted-instances.svg)](https://hackage.haskell.org/package/derive-lifted-instances) [![Build Status](https://github.com/sjoerdvisscher/derive-lifted-instances/workflows/Haskell-CI/badge.svg)](https://github.com/sjoerdvisscher/derive-lifted-instances/actions?query=workflow%3AHaskell-CI)

`derive-lifted-instances` generates type class instances using Template Haskell.

Below is an overview of what this library can do. If you could rewrite a class as one of the cases, and the listed constraints are satisfiable, then an instance can be derived. Note that when another instance of the class is required, this could also be a derived instance
(i.e. deriving is composable), in case you don't want that instance to actually exist.

|              | `class C x where alg :: f x -> x`
|--------------|---
| `x` iso `y`  | `(Functor f, C y)`
| `x=m`        | `(Foldable f, Monoid m)`
| `x=t a`      | `(Traversable f, Applicative t, C a)`
| `x=t a b`    | `(Traversable f, Biapplicative t, C a, C b)`
| `x` a record | `(Traversable f, All C flds)`

|              | `class C x where coalg :: x -> f x`
|--------------|---
| `x` iso `y`  | `(Functor f, C y)`
| `x=m`        | `(Pointed f)`
| `x=t a`      | `(Applicative f, Traversable t, C a)`
| `x=t a b`    | `(Applicative f, Bitraversable t, C a, C b)`
| `x` a record | `(Applicative f, All C flds)`

|              | `class C x where dialg :: f x -> g x`
|--------------|---
| `x` iso `y`  | `(Functor f, Functor g, C y)`
| `x=m`        | `(Foldable f, Pointed g, Monoid m)`
| `x=t a`      | `(Traversable f, Applicative g, Applicative t, Traversable t, C a)`
| `x=t a b`    | `(Traversable f, Applicative g, Biapplicative t, Bitraversable t, C a, C b)`
| `x` a record | `(Traversable f, Applicative g, All C flds)`
