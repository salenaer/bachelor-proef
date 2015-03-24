#lang cache/mutator

(allocator-setup "collector.rkt" 200 "directly-mapped-write-through.rkt")

(define (fib a)
  (case a
    ((0) 0)
    ((1) 1)
    (else (+ (fib (- a 2))(fib (- a 1))))))


