#lang racket/base

(require
 racket/class
 "utility.rkt" "interfaces.rkt" "base-classes.rkt"
 "type-class-monad.rkt" "monad.rkt" "functor.rkt" "applicative.rkt")

(provide MonadReaderClass% monad-reader ask asks local)

(define MonadReaderClass%
  (class MonadClass%
    (super-new)
    (inherit asks)

    (define/override (instance-base) MonadReader%)
    (define/override (instance-interface) MonadReader<%>)
    
    (define/public (ask-ask)
      (asks (λ (ctx) (send ctx ask))))

    (define/public (ask-asks)
      (asks (λ (ctx) (λ (f) (send ctx asks f)))))

    (define/public (ask-local)
      (asks (λ (ctx) (λ (f mx) (send ctx local f mx)))))))

(define monad-reader (new MonadReaderClass%))
(define get-ask (send monad-reader ask-ask))
(define get-asks (send monad-reader ask-asks))
(define get-local (send monad-reader ask-local))

(define ask get-ask)

(define (asks f)
  (let/tc ([asks get-asks])
    (return/tc (asks f))))

(define (local f cmx)
  (let/tc ([local get-local]
           [mx (injest~ cmx)])
    (return/tc (local f mx))))

(module+ test
  (require
   rackunit
   "base-classes.rkt")

  (define bare-reader
    (new
        (class ((compose derive-map/m derive-map/a) MonadReader%)
          (super-new)
          (define/override (input? x) #t)
          (define/override (in-context? x) (procedure? x))
          (define/override (map/f f mx) (λ (e) (f (mx e))))
          (define/override (return x) (λ (e) x))
          (define/override (join mmx) (λ (e) ((mmx e) e)))
          (define/override (ask) (λ (e) e))
          (define/override (local f mx) (λ (e) (mx (f e)))))))
  (check-equal? (run 'e (run bare-reader ask)) 'e)

  (check-equal?
   (run '((x . 3) (y . 4))
       (run bare-reader
           (let ([get-x (asks (λ (e) (cdr (assoc 'x e))))]
                 [get-y (asks (λ (e) (cdr (assoc 'y e))))])
             (let/m ([x get-x]
                     [y get-y])
               (return (+ x y))))))
   7))
