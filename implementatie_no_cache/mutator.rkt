#lang plai/mutator

(allocator-setup "collector.rkt" 200)

(define (fib a)
  (case a
    ((0) 0)
    ((1) 1)
    (else (+ (fib (- a 2))(fib (- a 1))))))


