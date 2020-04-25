#lang racket/base

(require
 racket/class
 "utility.rkt" "type-class-monad.rkt" "base-classes.rkt" "functor.rkt")

(provide
 ApplicativeClass% applicative return map/a)

(define ApplicativeClass%
  (class FunctorClass%
    (super-new)
    (inherit asks)
    
    (define/public (ask-return)
      (asks (λ (ctx) (λ (x) (send ctx return x)))))
    
    (define/public (ask-map/a)
      (asks (λ (ctx) (λ (mf mx) (send ctx map/a mf mx)))))))


(define applicative (new ApplicativeClass%))
(define get-return (send applicative ask-return))
(define get-map/a (send applicative ask-map/a))

(define (return x)
  (let/tc ([return get-return])
    (return/tc (return x))))

(define (map/a cmf cmx)
  (let/tc ([map/a get-map/a]
           [mf (injest~ cmf)]
           [mx (injest~ cmx)])
    (return/tc (map/a mf mx))))

(module+ test
  (require rackunit racket/function)

  (define (rappend xs ys)
    (if (null? xs) ys
      (rappend (cdr xs) (cons (car xs) ys))))

  (define list-applicative
    (new (class Applicative%
           (super-new)
           (define/override (in-context? x) (list? x))
           (define/override (map/f f xs) (map f xs))
           (define/override (return x) (list x))
           (define/override (map/a fs xs)
             (define (aux fs accum)
               (if (null? fs) (reverse accum)
                 (aux (cdr fs) (rappend (map/f (car fs) xs) accum))))
             (if (null? xs) '()
               (aux fs '()))))))

  (check-equal?
   (run list-applicative (return 'x))
   '(x))

  (check-equal?
   (run list-applicative
       (map/a (map/a (return (curry +)) '(1 2)) '(3 4)))
   '(4 5 5 6)))



