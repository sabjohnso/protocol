#lang racket/base

(require
 racket/class  math/flonum racket/class
 "main.rkt")

(provide flvector-functor FlVectorFunctor%)

(define FlVectorFunctor%
  (class* (send functor instance-base)
      ((send functor instance-interface))
    (super-new)
    (define/override (in-context? x) (flvector? x))
    (define/override (map/f f xs)
      (flvector-map f xs))))


(define flvector-functor (new FlVectorFunctor%))

(module+ test
  (require rackunit racket/contract math/flonum racket/fixnum)

  (define/contract (linspace a b n)
    (flonum? flonum? (and/c fixnum? (>=/c 2)) . -> . flvector?)
    (let* ([nm1 (fx- n 1)]
           [delta (fl/ (fl- b a) (fl nm1))]
           [pivot (fxquotient n 2)])
      (build-flvector n
        (λ (i) 
          (if (< i pivot)
              (fl+ a (fl* delta (fl i)))
              (fl- b (fl* delta (fl (fx- nm1 i)))))))))

  (linspace 0.0 1.0 17))
