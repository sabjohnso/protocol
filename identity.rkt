#lang racket/base

(require
 racket/class
 "main.rkt")

(define identity-monad
  (new (class* ((compose derive-map/f derive-map/a derive-join)
                (send monad instance-base))
           ((send monad instance-interface))
         (super-new)
         (define/override (in-context? x) #t)
         (define/override (show out) (display 'identity-monad out))
         (define/override (return x) x)
         (define/override (map/m f x) (f x)))))

(module+ test
  (require rackunit)

  (run identity-monad (map/f (λ (x) (* x x)) 4))
  (run identity-monad (map/m (λ (x) (* x x)) 4))
  (let/m ([x 3]
          [y 4])
    (return (+ x y))))
