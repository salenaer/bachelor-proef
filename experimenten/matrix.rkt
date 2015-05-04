#lang racket

(define (matrix-make row-count col-count)
  (let ((rows (make-vector row-count 0)))
    (define (loop x)
      (unless (= x row-count)
        (vector-set! rows x (make-vector col-count 0))
        (loop (+ x 1))))
    (loop 0)
    rows))

(define (matrix-print matrix)
  (let ((row-count (vector-length matrix)))
    (define (loop x)
      (unless (= x row-count)
        (display (vector-ref matrix x))(newline)
        (loop (+ x 1))))
    (loop 0)
    (newline)))

(define (matrix-set! matrix row col val)
  (vector-set! (vector-ref matrix row) col val))

(define (matrix-ref matrix row col)
  (vector-ref (vector-ref matrix row) col))

(define (matrix-product a b)
  (define max (vector-length b))
  (define row-count (vector-length a))
  (define col-count (vector-length (vector-ref b 0)))
  (define result (matrix-make row-count col-count))
  (define (calc-field i j)
    (define (loop ctr sum)
      (if (= ctr max)
          sum
          (loop (+ ctr 1)(+ sum (* (matrix-ref a i ctr)
                                   (matrix-ref b ctr j))))))
    (loop 0 0))
  (define (iter-cols ctr i)
    (if (= ctr col-count)
        'done
        (begin 
          (matrix-set! result i ctr (calc-field i ctr))
          (iter-cols (+ ctr 1) i))))
  (define (iter-rows ctr)
    (if (= ctr row-count)
        result
        (begin (iter-cols 0 ctr)
               (iter-rows (+ ctr 1)))))
  (iter-rows 0))

(define a (matrix-make 2 5))
(define (fill-row matrix i row)
  (vector-set! matrix i row))
(fill-row a 0 (vector 13 18 67 39 42))
(fill-row a 1 (vector 65 38 02 19 12))
(matrix-print a)

(define b (matrix-make 5 1))
(fill-row b 0 (vector 35))
(fill-row b 1 (vector 05))
(fill-row b 2 (vector 55))
(fill-row b 3 (vector 26))
(fill-row b 4 (vector 92))
(matrix-print b)

(define c (matrix-product a b))
(matrix-print c)