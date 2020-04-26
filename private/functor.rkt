#lang racket/base

(require
 racket/class
 "interfaces.rkt"
 "type-class-monad.rkt"
 "base-classes.rkt")

(provide
 functor FunctorClass% map/f digest)

(define FunctorClass%
  (class TypeClass%
    (super-new)
    (inherit asks)

    (define/public (instance-interface) Functor<%>)
    (define/public (instance-base) Functor%)
    
    (define/public (ask-map/f)
      (asks (λ (ctx) (λ (f mx) (send ctx map/f f mx)))))))

(define functor (new FunctorClass%))
(define get-map/f (send functor ask-map/f))
(define (map/f f cmx)
  (let/tc ([map/f get-map/f]
           [mx (injest~ cmx)])
    (return/tc (map/f f mx))))


;;
;; ... Testing
;;
(define (digest mx)
  (map/f injest (injest mx)))

(module+ test
  (require rackunit "utility.rkt")

  (define list-functor
    (new (class Functor%
           (super-new)
           (define/override (in-context? x) (list? x))
           (define/override (map/f f xs) (map f xs)))))
  
  (check-equal?
   (run list-functor
       (map/f (λ (x) (+ x x)) '(1 2 3 4)))
   '(2 4 6 8))

  (define identity-functor
    (new (class Functor%
           (super-new)
           (define/override (in-context? x) #t)
           (define/override (map/f f x) (f x)))))

  (check-equal?
   (run identity-functor
       (map/f (λ (x) (+ x x)) 3))
   6))
