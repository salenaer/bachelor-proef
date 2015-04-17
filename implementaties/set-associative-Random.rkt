#lang cache
(define cache:cache-size 5)
(define cache:set-size 3)
(define cache:block-size 5)

(define (cache:after-operation block-idx)
  (void))

;wat te doen voor een blok wordt overschreven in een bepaalde set => write through wordt gebruikt dus niets. 
(define (cache:before-eviction block-idx block-number source-adress)
  (void))

;wat te doen na write => write-through dus ook in main memory aanpassen
(define (cache:after-write location value block-idx)
  (child-write! location value))

(define (cache:calc-eviction-idx set-number)
  (random cache:set-size))
  
       
          