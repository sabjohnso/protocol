#lang racket/base

(require
 (for-syntax racket/base racket/syntax syntax/parse)
 racket/class racket/contract racket/generic
 "utility.rkt" "interfaces.rkt" "base-classes.rkt")

(provide
 TypeClass% let/tc ask/tc return/tc injest injest~)

(struct contextual
  ()
  #:transparent)

(struct contextual-return
  contextual
  (value)
  #:transparent
  #:methods gen:runnable
  ((define (run ctx this) (contextual-return-value this))))

(struct contextual-map/f
  contextual
  (proc value)
  #:transparent
  #:methods gen:runnable
  ((define/generic run/generic run)
   (define (run ctx this)
     ((contextual-map/f-proc this)
      (run/generic ctx (contextual-map/f-value this))))))

(struct contextual-join
  contextual
  (value)
  #:transparent
  #:methods gen:runnable
  ((define/generic run/generic run)
   (define (run ctx this)
     (run/generic ctx (run/generic ctx (contextual-join-value this))))))

(struct contextual-ask
  contextual
  ()
  #:transparent
  #:methods gen:runnable
  ((define (run ctx this) ctx)))

(struct contextual-local
  contextual
  (proc value)
  #:transparent
  #:methods gen:runnable
  ((define/generic run/generic run)
   (define (run ctx this)
     (run/generic (run/generic ctx (contextual-local-proc this))
                  (contextual-local-value this)))))

(define TypeClass%
  ((compose derive-map/m derive-map/a)
   (class MonadReader%
     (super-new)
     (define/public (get-class) this%)
     (define/override (in-context? x) (contextual? x))
     (define/override (input? x) (is-a? Context<%>))
     (define/override (map/f f mx) (contextual-map/f f mx))
     (define/override (return x)  (contextual-return x))
     (define/override (join mmx) (contextual-join mmx))
     (define/override (ask) (contextual-ask))
     (define/override (local f mx) (contextual-local f mx)))))


(define type-class (new TypeClass%))

(define ask/tc (send type-class ask))
(define (return/tc x)
  (send type-class return x))

(define injest return/tc)

(define (injest~ x)
  (if (contextual? x) x
    (injest x)))




(define-syntax let/tc
  (syntax-parser
   [(_ ([x:id mx:expr]) e:expr)
    #'(send type-class map/m (λ (x) e) mx)]

   [(_ ([x:id mx:expr] more-bindings:expr ...+) e:expr)
    #'(let/tc ([x mx]) (let/tc (more-bindings ...) e))]))


