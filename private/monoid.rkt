#lang racket/base

(require
 racket/class racket/contract/base
 "interfaces.rkt" "base-classes.rkt" "type-class-monad.rkt")

(provide monoid mempty mappend)


(define MonoidClass%
  (class TypeClass%
    (super-new)
    (inherit asks)
    
    (define/public (instance-interface)
      (interface (Context<%>)
        [mempty (([this context/c]) . ->i . [result (this) (in-context/c this)])]
        [mappend (([this context/c]
                   [mx (this) (in-context/c this)]
                   [my (this) (in-context/c this)])
                  . ->i . [result (this) (in-context/c this)])]))

    (define/public (instance-base)
      (class* Context%
          ((instance-interface))
        (super-new)
        (abstract mempty)
        (abstract mappend)))

    (define/public (get-mempty)
      (asks (λ (ctx) (send ctx mempty))))

    (define/public (get-mappend)
      (asks (λ (ctx) (λ (mx my) (send ctx mappend mx my)))))))


(define monoid (new MonoidClass%))

(define mempty
  (let/tc ([mempty (send monoid get-mempty)])
    (return/tc mempty)))

(define (mappend cmx cmy)
  (let/tc ([mappend (send monoid get-mappend)]
           [mx (injest~ cmx)]
           [my (injest~ cmy)])
    (return/tc (mappend mx my))))


(module+ test
  (require
   rackunit racket/function
   "utility.rkt")

  (define addition-monoid
    (new (class* (send monoid instance-base)
             ((send monoid instance-interface))
           (super-new)
           (define/override (in-context? x) (number? x))
           (define/override (mempty) 0)
           (define/override (mappend x y) (+ x y)))))


  (check-equal? (run addition-monoid (mappend 3 4)) 7)
  (check-equal? (run addition-monoid (mappend 3 mempty)) 3 )
  (check-equal? (run addition-monoid (mappend 3 mempty))
                (run addition-monoid (mappend mempty 3)))

  (define function-monoid
    (new (class* (send monoid instance-base)
             ((send monoid instance-interface))
           (super-new)
           (define/override (in-context? x) (procedure? x))
           (define/override (mempty) identity)
           (define/override (mappend mx my) (compose mx my)))))


  (define (twc x) (+ x x))
  (define (sqr x) (* x x))

  (check-equal? (run 3 (run function-monoid (mappend twc sqr))) 18)
  (check-equal? (run 3 (run function-monoid (mappend twc mempty)))
                (run 3 (run function-monoid (mappend mempty twc)))))
