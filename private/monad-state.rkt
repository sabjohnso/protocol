#lang racket/base

(require
 racket/class racket/contract/base
 "utility.rkt" "interfaces.rkt" "base-classes.rkt"
 "type-class-monad.rkt" "monad.rkt" "functor.rkt" "applicative.rkt" )

(define MonadStateClass%
  (class MonadClass%
    (super-new)
    (inherit asks)

    (define/public (instance-interface)
      (interface (Monad<%> ExecutableContext<%>)
        [get
         (([this context/c])
          . ->i . [res (this) (in-context/c this)])]

        [select
            (([this context/c]
              [f (this) ((input/c this) . -> . any/c)])
             . ->i . [result (this) (in-context/c this)])]
        
        [put
         (([this context/c]
           [s (this) (input/c this)])
          . ->i . [result (this) (in-context/c this)])]
        
        [modify
         (([this context/c]
           [f (this) ((input/c this) . -> . (input/c this))])
          . ->i . [result (this) (in-context/c this)])]))

    (define/public (instance-base)
      (class* Monad%
          ((instance-interface))
        (super-new)        
        (inherit map/f map/m)
        (abstract input?)
        (abstract get)
        (define/public (select f)
          (map/f f (get)))
        
        (abstract put)

        (define/public (modify f)
          (map/m (λ (s) (put s)) (select f)))))
    
    (define/public (ask-get)
      (asks (λ (ctx) (send ctx get))))

    (define/public (ask-select)
      (asks (λ (ctx) (λ (f) (send ctx select f)))))

    (define/public (ask-put)
      (asks (λ (ctx) (λ (s) (send ctx put s)))))

    (define/public (ask-modify)
      (asks (λ (ctx) (λ (f) (send ctx modify f)))))))


(define monad-state (new MonadStateClass%))

(define get
  (let/tc ([get (send monad-state ask-get)])
    (return/tc get)))
(define (select f)
  (let/tc ([select (send monad-state ask-select)])
    (return/tc (select f))))

(define (put s)
  (let/tc ([put (send monad-state ask-put)])
    (return/tc (put s))))

(define (modify f)
  (let/tc ([modify (send monad-state ask-modify)])
    (return/tc (modify f))))

(module+ test

  (require rackunit racket/match racket/undefined)

  (define bare-state-monad
    (new (class* ((compose derive-map/m derive-map/a) (send monad-state instance-base))
             ((send monad-state instance-interface))
           (super-new)
           
           (define/override (input? x) #t)

           (define/override (in-context? x) (procedure? x))

           (define/override (map/f f mx)
             (λ (s)
               (match-let ([(cons x s) (mx s)])
                 (cons (f x) s))))

           (define/override (return x)
             (λ (s) (cons x s)))
           
           (define/override (join mmx)
             (λ (s) (match-let ([(cons mx s) (mmx s)])
                      (mx s))))

           (define/override (get)
             (λ (s) ((return s) s)))

           (define/override (put s)
             (λ (_) (cons (void) s))))))



  (check-equal?
   (run 's (run bare-state-monad  (return 'x)))
   '(x . s))


  
  (check-equal?
   (run 's (run bare-state-monad (map/f (λ (x) (* x x)) (return 3))))
   '(9 . s))


  
  (check-equal?
   (run 's (run bare-state-monad get))
   '(s . s))


  
  (check-equal?
   (run 's
       (run bare-state-monad
           (let/m ([x get])
             (return x))))
   '(s . s))


  
  (check-equal?
   (let ([get-x (select (λ (s) (hash-ref s 'x undefined)))])
     (run (make-immutable-hash '((x . 3) (y . 4)))
         (run bare-state-monad
             (let/m ([x get-x])
               (return x)))))
   '(3 . #hash((x . 3) (y . 4))))



  (let ([get-x (select (λ (s) (hash-ref s 'x undefined)))]
        [get-y (select (λ (s) (hash-ref s 'y undefined)))]
        [get-result (select (λ (s) (hash-ref s 'result undefined)))]
        [set-result (λ (result) (modify (λ (s) (hash-set s 'result result))))])
    (check-equal?
     (run (make-immutable-hash '((x . 3) (y . 4)))
         (run bare-state-monad
             (let/m ([x get-x]
                     [y get-y]
                     [result get-result])
               (if (eq? result undefined)
                   (begin/m
                    (set-result (+ x y))
                    get)
                 get))))
     '(#hash((result . 7) (x . 3) (y . 4)) . #hash((result . 7) (x . 3) (y . 4)))))


  
  ((λ ()
     (begin
       (define/match (update input/state)
         [(`(1  . (,score . on)))  `(,(add1 score) . on)]
         [(`(-1 . (,score . on)))  `(,(sub1 score) . on)]
         [(`(on . (,score . off))) `(,score        . on)]
         [(`(off . (,score . on))) `(,score        . off)]
         [(`(_ . ,s))              s])
       (define/match (game inputs)
         [('()) get]
         [((list x xs ...))
          (begin/m
           (modify (λ (s) (update (cons x s))))
           (game xs))])
       (check-equal?
        (car
         (run '(0 . off)
             (run bare-state-monad
                 (game '(on 1 -1 -1 1 1 1 off)))))
        '(2 . off))))))






