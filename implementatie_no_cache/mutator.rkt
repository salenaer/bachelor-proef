#lang plai/mutator

(allocator-setup "collector.rkt" 200)

(define (fib a)
  (case a
    ((0) 0)
    ((1) 1)
    (else (+ (fib (- a 2))(fib (- a 1))))))

(define a (cons (cons 2 3)(cons 4 6)))
(define b (cons (cons a (cons 1 (cons a 5)))(cons 3 4)))
