#lang vector/mutator2

(allocator-setup "collector-vec.rkt" 200)

(define (fib a)
  (case a
    ((0) 0)
    ((1) 1)
    (else (+ (fib (- a 2))(fib (- a 1))))))


(define a (make-vector 4 5))
(vector-set! a 0 0)
(vector-set! a 1 1)

(vector-set! a 3 3)
(vector-set! a 2 (cons 1 2))
(vector-set! a 2 2)


