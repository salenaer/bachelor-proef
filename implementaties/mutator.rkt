#lang vector/mutator2

(allocator-setup "collector-vec.rkt" 200)

(define (fib a)
  (case a
    ((0) 0)
    ((1) 1)
    (else (+ (fib (- a 2))(fib (- a 1))))))


(define a (cons 1 fib))
(first a)

