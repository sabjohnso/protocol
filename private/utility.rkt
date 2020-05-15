#lang racket/base

(require
 racket/contract/base racket/class racket/generic)

(provide
 run gen:runnable
 (contract-out
  [predicate-method/c contract?]))

(define predicate-method/c (any/c . ->m . boolean?))

(define-generics runnable
  (run x runnable)
  #:fast-defaults
  ([procedure?
    (define (run x runnable)
      (runnable x))]))


(struct constantly
  (value)
  #:transparent
  #:methods gen:runnable
  ((define (run x this)
     (constantly-value this))))




