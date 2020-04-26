#lang racket/base

(require
 racket/class racket/contract/base
 "utility.rkt" "interfaces.rkt" "base-classes.rkt"
 "type-class-monad.rkt" "functor.rkt" "applicative.rkt" "monad.rkt" )

(provide monad-fail fail)

(define MonadFailClass%
  (class (send monad get-class)
    (super-new)
    (inherit asks)
    
    (define/override (instance-interface)
      (interface ((send monad instance-interface))
        [fail (([this context/c])
               . ->i . [result (this) (in-context/c this)])]))
    
    (define/override (instance-base)
      (class* (send monad instance-base)
          ((instance-interface))
        (super-new)
        (define/override (show out) (display '<MonadFail%> out))
        (abstract fail)))

    (define/public (ask-fail)
      (asks (λ (ctx) (send ctx fail))))))

(define monad-fail (new MonadFailClass%))

(define fail
  (let/tc ([fail (send monad-fail ask-fail)])
    (return/tc fail)))

(module+ test
  (require rackunit racket/match)

  (struct optional
    ()
    #:transparent)

  (struct nothing
    optional ()
    #:transparent)

  (struct some
    optional (value)
    #:transparent)
  
  (define optional-monad
    (new (class ((compose derive-map/m derive-map/a) (send monad-fail instance-base))
           (super-new)
           (define/override (in-context? x) (optional? x))
           (define/override (map/f f mx)
             (match mx
               [(some x) (some (f x))]
               [_ (nothing)]))
           (define/override (return x) (some x))
           (define/override (join mmx)
             (match mmx
               [(some (some x)) (some x)]
               [_  (nothing)]))
           (define/override (fail) (nothing)))))

  (define (safe-divide x y)
    (if (= y 0) (nothing)
      (some (/ x y))))

  (check-equal?
   (run optional-monad
       (let/m ([x (some 15)]
               [y (some 5)])
         (safe-divide x y)))
   (some 3))

  (check-equal?
   (run optional-monad
       (let/m ([x (nothing)]
               [y (some 5)])
         (safe-divide x y)))
   (nothing))

  (check-equal?
   (run optional-monad
       (let/m ([x (some 15)]
               [y (nothing)])
         (safe-divide x y)))
   (nothing))


  (check-equal?
   (run optional-monad
       (let/m ([x (nothing)]
               [y (nothing)])
         (safe-divide x y)))
   (nothing))

  (check-equal?
   (run optional-monad
       (let/m ([x (some 15)]
               [y (some 0)])
         (safe-divide x y)))
   (nothing)))

