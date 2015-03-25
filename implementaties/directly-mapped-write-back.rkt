#lang cache
(define cache:cache-size 3)
(define cache:set-size 1)
(define cache:block-size 1)

;dirty = true
(define dirty-bits (make-vector (* cache:cache-size cache:set-size) false))

(define (write-block-back read-adress write-adress)
  (define (loop ctr)
    (unless (= ctr cache:block-size)
      (child-write! (+ ctr write-adress) (cache-ref (+ ctr read-adress)))
      (loop (+ ctr 1))))
  (loop 0))

(define (cache:after-operation block-number)
  (void))

;wat te doen voor een blok wordt overschreven in een bepaalde set => write through wordt gebruikt dus niets. 
(define (cache:before-eviction block-idx block-number source-adress)
  (when (vector-ref dirty-bits block-idx);als data dirty is
      (write-block-back source-adress (* block-number cache:block-size)))
  (vector-set! dirty-bits block-idx false)) ;eviction gebeurt alleen als data wordt overschreven => nieuwe data is clean

;wat te doen na write => write-through dus ook in main memory aanpassen
(define (cache:after-write location value block-idx)
  (vector-set! dirty-bits block-idx true))

(define (cache:calc-eviction-idx set-number)
          0)       
          