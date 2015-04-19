#lang cache/mutator

;(allocator-setup "collector.rkt" 200 "set-associative-FIFO.rkt")
;(allocator-setup "collector.rkt" 200 "set-associative-Random.rkt")
;(allocator-setup "collector.rkt" 200 "set-associative-NMRU.rkt")
(allocator-setup "collector.rkt" 200 "set-associative-write-through.rkt")
;(allocator-setup "collector.rkt" 200 "directly-mapped-write-through.rkt")
;(allocator-setup "collector.rkt" 150 "directly-mapped-write-back.rkt")


(define (fib a)
  (case a
    ((0) 0)
    ((1) 1)
    (else (+ (fib (- a 2))(fib (- a 1))))))


