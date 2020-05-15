#lang racket/base

(require
 racket/class racket/contract racket/future racket/function
 "main.rkt")

(provide future-monad FutureMonad%)


(define FutureMonad%
  (class* ((compose derive-map/a derive-map/m)
           (send monad instance-base))
      ((send monad instance-interface))
    (super-new)

    (define/override (in-context? x) (future? x))
    

    (define/override (map/f f mx)
      (future (thunk (f (touch mx)))))

    (define/override (return x)
      (future (thunk x)))

    (define/override (join mmx)
      (future (thunk (touch (touch mmx)))))))

(define future-monad
  (new FutureMonad%))


(module+ test
  (require rackunit))




(define (sqr x y z e)
  (* x x))
