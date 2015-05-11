#lang cache/mutator

;(allocator-setup "collector.rkt" 200 "set-associative-FIFO.rkt")
;(allocator-setup "collector.rkt" 200 "set-associative-Random.rkt")
;(allocator-setup "collector.rkt" 200 "set-associative-NMRU.rkt")
;(allocator-setup "collector.rkt" 200 "set-associative-write-through.rkt")
;(allocator-setup "collector-non-moving.rkt" 200 "directly-mapped-write-through.rkt")
(allocator-setup "collector-non-moving.rkt" 200 "directly-mapped-write-back.rkt")

(define (map vector proc)
  (map-iter vector proc (vector-length vector) 0))

(define (map-iter vector proc end ctr)
  (if (= ctr end)
      vector
      (map-iter (proc vector ctr) proc end (+ ctr 1))))

(define (matrix-make row-count col-count)
  (map (make-vector row-count 0) (lambda (rows ctr)
                                   (vector-set! rows ctr (make-vector col-count 0))
                                   rows)))

(define (matrix-set! matrix row col val)
  (vector-set! (vector-ref matrix row) col val))

(define (matrix-ref matrix row col)
  (vector-ref (vector-ref matrix row) col))

(define (matrix-print matrix)
  (map matrix (lambda (rows ctr)
                      (display (vector-ref rows ctr))(newline)
                                   rows))
  (newline)
  (void))

(define a (matrix-make 2 5))
(matrix-set! a 0 0 13)
(matrix-set! a 0 1 18)
(matrix-set! a 0 2 67)
(matrix-set! a 0 3 39)
(matrix-set! a 0 4 42)
(matrix-set! a 1 0 65)
(matrix-set! a 1 1 38)
(matrix-set! a 1 2 02)
(matrix-set! a 1 3 19)
(matrix-set! a 1 4 12)
(matrix-print a)

(define b (matrix-make 5 1))
(matrix-set! b 0 0 35)
(matrix-set! b 1 0 05)
(matrix-set! b 2 0 55)
(matrix-set! b 3 0 26)
(matrix-set! b 4 0 92)
(matrix-print b)

(define (matrix-product a b)
  (let ((max (vector-length b))
        (row-count (vector-length a))
        (col-count (vector-length (vector-ref b 0))))
    (matrix-product-iter-rows (matrix-make row-count col-count) max a b row-count col-count 0)))

(define (matrix-product-iter-rows result max a b row-count col-count row)
    (if (= row row-count)
       result
        (begin (matrix-product-iter-cols result  max a b col-count row 0)
               (matrix-product-iter-rows result  max a b row-count col-count (+ row 1)))))

(define (matrix-product-iter-cols result  max a b col-count row col)
   (if (= col col-count)
        'done
        (begin 
         (matrix-set! result row col (matrix-product-calc-field  max a b row col 0 0))
         (matrix-product-iter-cols result  max a b col-count row (+ col 1)))))

(define (matrix-product-calc-field  max a b row col ctr sum)
    (if (= ctr max)
         sum
          (matrix-product-calc-field max a b row col 
                                     (+ ctr 1)
                                     (+ sum (* (matrix-ref a row ctr)
                                               (matrix-ref b ctr col))))))

(define c (matrix-product a b))
(matrix-print c)

