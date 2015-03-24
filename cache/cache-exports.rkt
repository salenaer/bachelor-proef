#lang racket

(provide (all-defined-out))

;grootte van blok + associativiteit
(define cache-size false) 
(define set-size false)  
(define block-size false) 

(define (set-cache-size! size)
  (set! cache-size size))
(define (set-set-size! size)
  (set! set-size size))
(define (set-block-size! size)
  (set! block-size size))

;write-strategy
(define after-operation false)
(define after-write false)

(define (set-cache:after-operation! proc)
  (set! after-operation proc))
(define (set-cache:after-write! proc)
  (set! after-write proc))

;eviction strategy
(define calc-eviction-idx false)
(define before-eviction false)

(define (set-cache:calc-eviction-idx! proc)
  (set! calc-eviction-idx proc))
(define (set-cache:before-eviction! proc)
  (set! before-eviction proc))


                                     