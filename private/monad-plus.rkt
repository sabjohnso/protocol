#lang racket/base

(require
 racket/class racket/contract/base
 "utility.rkt" "interfaces.rkt" "base-classes.rkt"
 "type-class-monad.rkt" "functor.rkt" "applicative.rkt" "monad.rkt" "monad-fail.rkt")


(provide monad-plus mzero mplus </>)

(define MonadPlusClass%
  (class (send monad-fail get-class)
    (super-new)
    (inherit asks)

    (define/override (instance-interface)
      (interface ((send monad-fail instance-interface))
        [mzero (([this context/c])
                . ->i . [result (this) (in-context/c this)])]
        [mplus (([this context/c]
                 [mx (this) (in-context/c this)]
                 [my (this) (in-context/c this)])
                . ->i . [result (this) (in-context/c this)])]))

    (define/override (instance-base)
      (class* (send monad-fail instance-base)
          ((instance-interface))
        (super-new)
        (define/override (show out) (display '<MonadPlus%> out))
        (define/override (fail) (mzero))
        (abstract mzero)
        (abstract mplus)))


    (define/public (ask-mzero)
      (asks (λ (ctx) (send ctx mzero))))
    
    (define/public (ask-mplus)
      (asks (λ (ctx) (λ (mx my) (send ctx mplus mx my)))))))

(define monad-plus (new MonadPlusClass%))

(define mzero
  (let/tc ([mzero (send monad-plus ask-mzero)])
    (return/tc mzero)))

(define (mplus cmx cmy)
  (let/tc ([mplus (send monad-plus ask-mplus)]
           [mx (injest~ cmx)]
           [my (injest~ cmy)])
    (return/tc (mplus mx my))))

(define (</> . mxs)
  (define (aux mxs accum)
    (if (null? mxs) accum
      (aux (cdr mxs) (mplus accum (car mxs)))))
  (aux mxs mzero))


(module+ test
  (require rackunit)

  (define list-monad
    (new (class* ((compose derive-map/m derive-map/a)
                  (send monad-plus instance-base))
             ((send monad-plus instance-interface))
           (super-new)
           (define/override (in-context? x) (list? x))
           (define/override (map/f f xs) (map f xs))
           (define/override (return x) (list x))
           (define/override (join xss) (apply append xss))
           (define/override (mzero) '())
           (define/override (mplus xs ys) (append xs ys)))))

  (check-equal? (run list-monad (mplus '(1 2) '(3 4))) '(1 2 3 4))
  (check-equal?
   (run list-monad (mplus '(1 2) (mplus '(3 4) '(5 6))))
   (run list-monad (mplus (mplus '(1 2) '(3 4)) '(5 6))))

  (check-equal?
   (run list-monad (mplus '(1 2) mzero))
   '(1 2))

  (check-equal?
   (run list-monad (mplus '(1 2) mzero))
   (run list-monad (mplus mzero '(1 2)))))
