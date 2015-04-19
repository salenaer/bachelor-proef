#lang cache
(require rackunit)
(define cache:cache-size 5)
(define cache:set-size 3)
(define cache:block-size 5)

(define time-stamps (make-vector (* cache:cache-size cache:set-size) false))

(define (cache:after-operation block-idx)
  (vector-set! time-stamps block-idx (current-inexact-milliseconds)))

;wat te doen voor een blok wordt overschreven in een bepaalde set => write through wordt gebruikt dus niets. 
(define (cache:before-eviction block-idx block-number source-adress)
  (void))

;wat te doen na write => write-through dus ook in main memory aanpassen
(define (cache:after-write location value block-idx)
  (child-write! location value))

(define (cache:calc-eviction-idx set-number)
     (define start (* set-number cache:set-size))
     (define (loop ctr lowest answer)
       (if (eq? ctr cache:set-size)
           answer
           (let ((new-value (vector-ref time-stamps (+ start ctr))))
             (cond ((not new-value) ctr)
                   ((< new-value lowest)(loop (+ ctr 1) new-value ctr))
                   (else (loop (+ ctr 1) lowest answer))))))
  (loop 0 +inf.0 +inf.0))

(define (test-LRU)
  ;set 1
  (vector-set! time-stamps 0 (current-inexact-milliseconds))
  (vector-set! time-stamps 2 (current-inexact-milliseconds))
  ;set 2
  (vector-set! time-stamps 3 1)
  (vector-set! time-stamps 4 2)
  (vector-set! time-stamps 5 3)
  ;set 3
  (vector-set! time-stamps 6 9)
  (vector-set! time-stamps 7 #f)
  (vector-set! time-stamps 8 1)
  ;set 4
  (vector-set! time-stamps 9 9)
  (vector-set! time-stamps 10 8)
  (vector-set! time-stamps 11 7)
  ;set 5
  (vector-set! time-stamps 12 (current-inexact-milliseconds))
  (vector-set! time-stamps 13 (current-inexact-milliseconds))
  (vector-set! time-stamps 14 (current-inexact-milliseconds))
  (check-equal? (cache:calc-eviction-idx 0) 1)
  (check-equal? (cache:calc-eviction-idx 1) 0)
  (check-equal? (cache:calc-eviction-idx 2) 1)
  (check-equal? (cache:calc-eviction-idx 3) 2)
  (check-equal? (cache:calc-eviction-idx 4) 0))
  
       
          