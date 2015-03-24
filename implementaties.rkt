#lang cache

;get-cache-line vind op basis van het heap-adress van data, het eerste cache-adress van de cache set van de data
;placement directly-mapped => memory-size mod (cache-size / set-size) (set-size hier 2: home-adress + value)
(define (cache:get-cache-line)
  (modulo value (/ (cache-size) set-size)))

(define (cache:get-cache-location cache-line location)
  (define (loop ctr)
    (cond ((eq? ctr set-size) #f)
          ((eq? (cache-ref ctr) location) ctr)
          (loop (+ ctr tupple-size))))
  (loop cache-line))

(define tupple-size 2) ;grootte van data, hier origineel adress, data
(define set-size tupple-size) ;grootte van sets direct mapped, dus evengroot als tupple-size

;(provide/contract [correct-cache-size? (any/c . -> . boolean?)])
;(define (correct-cache-size? v) 
;  (and (exact-nonnegative-integer? v) (eq? 0 (modulo v set-size))))



;kijk op basis van de line en de gezochte locatie waar de waarde staat als die er is.
;hier is elke set 2 lang dus direct gevonden. 


;vind op basis van de set de blok om te verwijderen
(define (get-eviction-adress cache-set)
  cache-set) ;directly-mapped 1 blok per set, verwijder die blok

;wat te doen voor een blok wordt overschrevenin bepaalde set => write through wordt gebruikt dus niets. 
(define (before-eviction set)
  'nop)

;wat te doen na write => write-through dus ook in main memory aanpassen
(define (after-write location value)
  (heap-set! location value)) 