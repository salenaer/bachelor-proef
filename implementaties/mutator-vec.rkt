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
(vector-set! a 2 2)
(vector-set! a 3 3)
(define b (make-vector 4 5))
(vector-set! b 0 0)
(vector-set! b 1 1)
(vector-set! b 2 2)
(vector-set! b 3 3)
(define (turn-pointer) (vector-set! a 2 (cons 1 2)))
(define (add-pointer)(vector-set! a 3 b))
(define (remove-pointer)(vector-set! a 3 3))
(define (turn-flat)(vector-set! a 2 2))
(vector-set! a 2 2)

(define c (cons 1 (cons 2 (cons 3 (cons 4 2)))))