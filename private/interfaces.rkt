#lang racket/base

(require
 racket/class racket/contract "utility.rkt")


(provide (all-defined-out))


(define Context<%>
  (interface ()
    [in-context? predicate-method/c]))

(define (in-context/c this)
  (make-contract
   #:name '(in-context/c)
   #:first-order (λ (x) (send this in-context? x))))

(define context/c (is-a?/c Context<%>))

(define Functor<%>
  (interface (Context<%>)
    [map/f (([this context/c] [f (any/c . -> . any/c)] [mx (this) (in-context/c this)])
           . ->i . [return (this) (in-context/c this)])]))

(define Applicative<%>
  (interface (Functor<%>)
    [return (([this context/c]
              [x any/c])
             . ->i . [result (this) (in-context/c this)])]
    [map/a (([this context/c]
             [mf (this) (in-context/c this)]
             [mx (this) (in-context/c this)])
            . ->i . [result (this) (in-context/c this)])]))

(define Monad<%>
  (interface (Applicative<%>)
    [join (([this context/c]
            [mmx (this) (in-context/c this)])
           . ->i . [result (this) (in-context/c this)])]
    [map/m (([this context/c]
             [f (this) (any/c . -> . (in-context/c this))]
             [mx (this) (in-context/c this)])
            . ->i . [result (this) (in-context/c this)])]))


(define ExecutableContext<%>
  (interface (Context<%>)
    [input? predicate-method/c]))

(define executable-context/c (is-a?/c ExecutableContext<%>))

(define/contract (input/c this)
    (executable-context/c . -> . contract?)
  (make-contract
   #:name 'input/c
   #:first-order (λ (x) (send this input? x))))


(define MonadReader<%>
  (interface (Monad<%> ExecutableContext<%>)
    [ask
     (([this context/c])
      . ->i . [result (this) (in-context/c this)])]

    [asks
     (([this context/c]
       [f (this) ((input/c this) . -> . any/c)])
      . ->i . [result (this) (in-context/c this)])]

    [local
        (([this executable-context/c]
          [f (this) ((input/c this) . -> . any/c)]
          [mx (this) (in-context/c this)])
         . ->i . [result (this) (in-context/c this)])]))



