#lang racket/base

(require
 racket/class racket/match
 "main.rkt")

(provide reader-monad)

(struct env
  (proc)
  #:methods gen:runnable
  ((define (run x this)
     ((env-proc this) x))))

(define reader-monad
  (new (class* ((compose derive-map/a derive-map/m) (send monad-reader instance-base))
           ((send monad-reader instance-interface))
         (super-new)
         (define/override (input? x) #t)
         (define/override (in-context? x) (env? x))
         (define/override (map/f f mx)
           (env (λ (e) (f (run e mx)))))
         (define/override (return x)
           (env (λ (e) x)))
         (define/override (join mmx)
           (env (λ (e) (run e (run e mmx)))))
         (define/override (ask)
           (env (λ (e) e)))
         (define/override (local f mx)
           (env (λ (e) (run (f e) mx)))))))

(module+ test
  (require rackunit racket/undefined)

  (check-equal? (run 'e (reader-monad (return 'x))) 'x)
  (check-equal? (run 'e (reader-monad ask)) 'e)

  (let ([get-x (asks (λ (e) (hash-ref e 'x undefined)))]
        [get-y (asks (λ (e) (hash-ref e 'y undefined)))])
    (check-equal?
     (run (make-immutable-hash '((x . 3) (y . 4)))
       (reader-monad
        (let/m ([x get-x]
                [y get-y])
          (return (+ x y)))))
     7)))
