#lang scribble/manual
@require[@for-label[protocol racket/base]]

@title{protocol}
@author{Samuel B. Johnson}

@defmodule[protocol]

A protocol system that is conceptually similar to Haskell's Type
Classes or Racket's racket/generic library, but also, significantly
different through the decoupling of the protocols and argument types
or predicates. There are a number of built-in protocols that cover a
wide range of common use cases.

@include-section["built-in.scrbl"]

@include-section["create.scrbl"]

@include-section["technical-background.scrbl"]
