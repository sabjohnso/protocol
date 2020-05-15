protocol
==========

A protocol system for racket


### About

This library implements a protocol system for Racket that is
conceptually similar to Racket's generic functions or Haskell's type
classes, but diverges significantly with regard to method resolution.
Rather than bind method resolution to predicates as in Racket's
generic functions or types as in Haskell's type classes, herein,
computations are built lazily and the protocol instance is threaded
through the computations as a context in which they are being
evaluated. In addition to the protocol system itself, this library
provides a number of built-in protocols and and instances of those
protocols for common use cases: monoids, functors, applicatives
monads, comonads, etc

```scheme

(define functor
  (protocol contextual ([m : (type . -> . type)])
    (declare (map/f [a : type] [b : type])
      ((a . -> . b) (m a) . -> . (m b)))))
      
(define applicative
  (protocol functor ([m : (type . -> . type)])
    (declare (return [a : type])
      (a . -> . (m a)))

    (declare (map/a [a : type] [b : type])
      ((m (a . -> . b)) (m a) . -> . (m b)))))

(define monad
  (protocol applicative ([m : (type . -> . type)])
     (declare (return ([a : type]))
       (a . -> . (m a)))
     (declare (map/m  ([a : type] [b : type]))
       ((a . -> . (m b)) (m a) . -> . (m b)))))






```

```scheme

(define (+/m mx my)
  (let/m ([x mx]
          [y my])
    (return (+ mx my))))
  
(identity-monad
  (+/m 2 3))
;; => 5
  
(list-monad
  (+/m '(1 2) '(3 4)))
;; => '(4 5 5 6)
```
