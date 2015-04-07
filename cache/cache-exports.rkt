#lang racket

(provide (all-defined-out))

;grootte van blok + associativiteit
(define cache-size false) ;hoeveel sets gaan er in de cache
(define set-size false)   ;hoeveel blokken gaan er in elke set
(define block-size false) ;hoeveel datawaarden gaan er in elk blok

(define (set-cache-size! size)
  (set! cache-size size))
(define (set-set-size! size)
  (set! set-size size))
(define (set-block-size! size)
  (set! block-size size))

;write-strategy
(define after-operation false) ;krijgt de index van het blok in de cache, moet ervoor zorgen dat de write strategy correct functioneerd
;krijgt het adress om naar te schrijven, de waarde om weg te schrijven en de index van het blok in de cache, 
;moet ervoor zorgen dat de write strategy correct functioneerd
(define after-write false)     

(define (set-cache:after-operation! proc)
  (set! after-operation proc))
(define (set-cache:after-write! proc)
  (set! after-write proc))

;eviction strategy
(define calc-eviction-idx false);krijgt de index van de set, moet de index van een blok binnen de set terug geven dus 0 tot setsize
;krijgt de index van het blok om te verwijderen in de cache, het blok-number en het adress van de eerste waarde waarop dit blok data te vinden is
(define before-eviction false)

(define (set-cache:calc-eviction-idx! proc)
  (set! calc-eviction-idx proc))
(define (set-cache:before-eviction! proc)
  (set! before-eviction proc))


                                     