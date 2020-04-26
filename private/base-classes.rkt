#lang racket/base

(require
 racket/contract/base racket/class
 (only-in racket/function identity)
 "interfaces.rkt")

(provide (all-defined-out))

(define Context%
  (class* ((compose derive-printable abstract-show) object%)
      (Context<%> Callable<%>)
    (super-new)
    (define/override (show out) (display '<Context%> out))
    (define/public (call f)
      (f this))
    (abstract in-context?)))

(define Functor%
  (class* Context%
      (Functor<%>)
    (super-new)
    (define/override (show out) (display '<Functor%> out))
    (abstract map/f)))

(define Applicative%
  (class* Functor%
      (Applicative<%>)
    (super-new)
    (define/override (show out) (display '<Applicative%> out))
    (abstract return)
    (abstract map/a)))

(define Monad%
  (class* Applicative%
      (Monad<%>)
    (super-new)
    (define/override (show out) (display '<Monad%> out))
    (abstract join)
    (abstract map/m)))


(define MonadReader%
  (class* Monad%
      (MonadReader<%>)
    (super-new)
    (inherit map/f)
    (define/override (show out) (display '<MonadReader%> out))
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
