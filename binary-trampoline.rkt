#lang racket/base


(require
 racket/class racket/match racket/function
 "main.rkt")

(provide binary-trampoline BinaryTrampoline%)

(module+ test
  (require rackunit))

(struct app
  (fun  args)
  #:property prop:procedure
  (λ (this)
    (apply (app-fun this) (app-args this))))

(define Y (λ (f) (f f)))

(define BinaryTrampoline%
  (class* ((compose derive-map/f derive-map/a derive-join)
           (send monad-plus instance-base))
      ((send monad-plus instance-interface))
    (super-new)
    (define (rappend xs ys)
      (if (null? xs) ys
          (rappend (cdr xs) (cons (car xs) ys))))
    
    (define/override (in-context? x) (or (pair? x) (procedure? x) (null? x)))
    (define/override (mzero) '())
    (define/override (mplus xs ys)
      (match xs
        [(? procedure?) (thunk (mplus ys (xs)))]
        ['() ys]
        [`(,x . ,xs) (cons x (mplus xs ys))]))
    (define/override (return x) `(,x . ()))
    (define/override (map/m f xs)
      (match xs
        [(? procedure?) (thunk (map/m f (xs)))]
        ['() '()]
        [`(,x . ,xs) (mplus (f x) (map/m f xs))]))))
(define binary-trampoline
  (new BinaryTrampoline%))
