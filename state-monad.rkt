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

(define (state-exec s mx)
  (car (run s mx)))

(define (state-eval s mx)
  (cdr (run s mx)))

(module+ test
  (require rackunit)

  (check-equal?
   (run 's (state-monad (return 'x)))
   '(x . s))

  (letrec ([update (λ (x s)
                     (match (cons x s)
                       [(cons 'on  `(,score . off)) `(,score . on)]
                       [(cons'off `(,score . on))  `(,score . off)]
                       [(cons 1    `(,score . on))  `(,(add1 score) . on)]
                       [(cons -1   `(,score . on))  `(,(sub1 score) . on)]
                       [(cons _ s) s]))]
           [game (match-lambda
                   ['() get]
                   [(cons x xs) (let/m ([s get])
                                  (put (update x s))
                                  (game xs))])]
           [empty-state '(0 . off)])
    (check-equal?
     (state-exec empty-state (state-monad (game '(on 1 1 -1 -1 1 1 off))))
     '(2 . off))))


