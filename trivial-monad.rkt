#lang racket/base

(require
 racket/class
 "main.rkt")

(provide trivial-monad)

(define (trivial-monad constructor accessor predicate?)
  (new (class* ((compose derive-map/f derive-map/a derive-join)
                (send monad instance-base))
           ((send monad instance-interface))
         
         (super-new)
         
         (define/override (in-context? x) (predicate? x))
         (define/override (return x) (constructor x))
         (define/override (map/m f mx)
           (f (accessor mx))))))
