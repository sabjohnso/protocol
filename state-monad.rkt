#lang racket/base


(require
 racket/class racket/match
 "private/utility.rkt" "main.rkt")

(provide state-monad)

(struct stateful
  (proc)
  #:methods gen:runnable
  ((define (run x this)
     ((stateful-proc this) x))))

(define state-monad
  (new (class* ((compose derive-map/a derive-map/m) (send monad-state instance-base))
           ((send monad-state instance-interface))
         (super-new)
         (define/override (in-context? x) (stateful? x))
         (define/override (input? x) #t)
         (define/override (map/f f mx)
           (stateful (λ (s) (match (run s mx) [(cons x s) (cons (f x) s)]))))
         (define/override (return x)
           (stateful (λ (s) (cons x s))))
         (define/override (join mmx)
           (stateful (λ (s) (match (run s mmx) [(cons mx s) (run s mx)]))))
         (define/override (get)
           (stateful (λ (s) (cons s s))))
         (define/override (put s)
           (stateful (λ (_) (cons (void) s)))))))
