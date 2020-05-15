#lang racket/base

(require
 racket/class
 "main.rkt")

(provide list-monad list-monoid)

(define (rappend xs ys)
  (if (null? xs) ys
    (rappend (cdr xs) (cons (car xs) ys))))

(define list-monad
  (new (class* ((compose derive-join derive-map/f derive-map/a)
                (send monad-plus instance-base))
           ((send monad-plus instance-interface))
         (super-new)
         (define/override (show out) (display 'list-monad out))
         (define/override (in-context? x) (list? x))
         (define/override (return x) (list x))
         (define/override (map/m f xs)
           (define (aux xs accum)
             (if (null? xs) (reverse accum)
               (aux (cdr xs) (rappend (f (car xs)) accum))))
           (aux xs '()))
         (define/override (mzero) '())
         (define/override (mplus xs ys) (append xs ys)))))

(define list-monoid
  (new (class* (send monoid instance-base)
           ((send monoid instance-interface))
         (super-new)
         (define/override (show out) (display 'list-monoid out))
         (define/override (in-context? x) (list? x))
         (define/override (mempty) '())
         (define/override (mappend xs ys) (append xs ys)))))





(module+ test
  (require rackunit)

  (check-equal? (run list-monad (return 'x)) '(x))
  (check-equal? (run list-monad (map/m list (list 1 2))) (list 1 2))  
  (check-equal? (run list-monad (map/f (λ (x) (+ x x)) (list 1 2))) '(2 4))

  (check-equal?
   (run list-monad
       (map/a (list (λ (x) (+ x x))
                    (λ (x) (* x x)))
              (list 1 2)))
   '(2 4 1 4))

  (check-equal?
   (run list-monad
       (let/m ([x '(1 2)]
               [y '(3 4)])
         (return (+ x y))))
   '(4 5 5 6))

  (check-equal?
   (run list-monad (</> '(1 2) '(3 4) '(5 6)))
   '(1 2 3 4 5 6))

  (check-equal?
   (list-monad
    ('(1 2 3 4) . >>= . list))
   '(1 2 3 4))

  (check-equal?
   (list-monad ('(1 2) . >>= . (λ (x) (list x x))))
   '(1 1 2 2))


  (check-equal?
   (list-monad
    (( list list (λ (x) (list x x)) (λ (x) (list x x)) . >=> . list)
     1))
   '(1 1 1 1))

  (let* ([dup (λ (x) (list x x))]
         [f (dup . >=> . list)]
         [g (list . >=> . dup)])
    (check-equal? (list-monad (f 'x)) (list-monad (g 'x))))


  (let* ([f (λ (x) (list x x))]
         [g (λ (x) (list (add1 x)))]
         [h (λ (x) (list (format "~A" x)))])
    
    (check-equal?
     (list-monad (((f . >=> . g) . >=> . h) 3))
     (list-monad ((f . >=> . (g . >=> . h)) 3)))))

