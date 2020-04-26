#lang racket/base

(require
 (for-syntax racket/base racket/syntax syntax/parse)
 racket/class
 "utility.rkt" "interfaces.rkt" "base-classes.rkt"
 "type-class-monad.rkt" "functor.rkt" "applicative.rkt")

(provide MonadClass% monad join map/m let/m begin/m >=> <=< >>=)

(define MonadClass%
  (class ApplicativeClass%
    (super-new)
    (inherit asks)

    (define/override (instance-interface) Monad<%>)
    (define/override (instance-base) Monad%)
    
    (define/public (ask-join)
      (asks (λ (ctx) (λ (mmx) (send ctx join mmx)))))

    (define/public (ask-map/m)
      (asks (λ (ctx) (λ (f mx) (send ctx map/m f mx)))))))

(define monad (new MonadClass%))
(define get-join (send monad ask-join))
(define get-map/m (send monad ask-map/m))

(define (join cmcmx)
  (let/tc ([ctx ask/tc]
           [join get-join]
           [mmx (map/f (λ (cmx) (run ctx cmx)) (map/f injest~ (injest~ cmcmx)))])
    (return/tc (join mmx))))

(define (map/m f cmx)
  (let/tc ([ctx ask/tc]
           [map/m get-map/m]
           [mx (injest~ cmx)])
    (return/tc
      (map/m (λ (x) (run ctx (injest~ (f x))))             
             mx))))


(define-syntax begin/m
  (syntax-parser
   [(_ e:expr) #'e]
   [(_ e:expr es:expr ...+)
    (with-syntax ([ignore (generate-temporary 'ignore)])
        #'(map/m (λ (ignore) (begin/m es ...)) e))]))

(define-syntax let/m
  (syntax-parser
   [(_ ([x:id mx:expr]) es:expr ...+)
    #'(map/m (λ (x) (begin/m es ...)) mx)]
   
   [(_ ([x:id mx:expr] more-bindings:expr ...+) es:expr ...+)
    #'(let/m ([x mx]) (let/m (more-bindings ...) es ...))]))

(define (>=> . fs)
  (define (aux fs accum)
    (if (null? fs) accum
      (aux (cdr fs) (λ (x) (map/m accum ((car fs) x))))))
  (aux fs return))

(define (<=< . fs)
  (apply >=> (reverse fs)))

(define (>>= mx f)
  (map/m f mx))

(module+ test
  (require
   rackunit
   "base-classes.rkt")

  (define list-monad
    (new (class ((compose derive-map/m derive-map/a) Monad%)
           (super-new)
           (define/override (in-context? x) (list? x))
           (define/override (map/f f xs) (map f xs))
           (define/override (return x) (list x))
           (define/override (join xss)
             (apply append xss)))))

  (check-equal?
   (run list-monad
       (join (list (list 1 2) (list 3 4))))
   '(1 2 3 4))

  (check-equal?
   (run list-monad
       (let/m ([x '(1 2)])
         (return x)))
   '(1 2))

  (check-equal?
   (run list-monad
       (let/m ([x '(1 2)]
               [y '(3 4)])
         (return (+ x y))))
   '(4 5 5 6))


  (define identity-monad
    (new (class ((compose derive-join derive-map/a derive-map/f) Monad%)
           (super-new)
           (define/override (in-context? x) #t)
           (define/override (return x) x)
           (define/override (map/m f x) (f x)))))

  (check-equal? (run identity-monad (join 3)) 3)

  (check-equal?
   (run identity-monad
       (let/m ([x 3]
               [y 4])
         (return (+ x y))))
   7))



