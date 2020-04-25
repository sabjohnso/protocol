#lang racket/base

(require
 racket/contract/base racket/class
 (only-in racket/function identity)
 "interfaces.rkt")

(provide (all-defined-out))

(define Context%
  (class* object%
      (Context<%>)
    (super-new)
    (abstract in-context?)))

(define Functor%
  (class* Context%
      (Functor<%>)
    (super-new)
    (abstract map/f)))

(define Applicative%
  (class* Functor%
      (Applicative<%>)
    (super-new)
    (abstract return)
    (abstract map/a)))

(define Monad%
  (class* Applicative%
      (Monad<%>)
    (super-new)
    (abstract join)
    (abstract map/m)))


(define MonadReader%
  (class* Monad%
      (MonadReader<%>)
    (super-new)
    (inherit map/f)
    (abstract input?)
    (abstract ask)
    (abstract local)
    (define/public (asks f)
      (map/f f (ask)))))


(define derive-map/m
  (mixin (Monad<%>) (Monad<%>)
    (super-new)
    (inherit join map/f)
    (define/override (map/m f mx)
      (join (map/f f mx)))))


(define derive-map/a
  (mixin (Monad<%>) (Monad<%>)
    (super-new)
    (inherit map/m map/f)
    (define/override (map/a mf mx)
      (map/m (λ (f) (map/f f mx)) mf))))

(define derive-join
  (mixin (Monad<%>) (Monad<%>)
    (super-new)
    (inherit map/m)
    (define/override (join mmx)
      (map/m identity mmx))))

(define derive-map/f
  (mixin (Monad<%>) (Monad<%>)
    (super-new)
    (inherit return map/m)
    (define/override (map/f f mx)
      (map/m (λ (x) (return (f x))) mx))))
