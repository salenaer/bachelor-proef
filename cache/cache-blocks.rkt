#lang racket

(define memory-size 300)  

;from user
(define cache-size 100) ;aantal values die ik wil kunnen opslaan
(define block-size 5)   ;aantal waarden in een block
(define set-size 1)     ;aantal blokken per set
(define max-blocks (/ cache-size block-size))

;standaard
(define cache (make-vector (+ cache-size max-blocks)))
(define memory (make-vector memory-size))

(define (cache-ref x)
  (vector-ref cache x))

(define (cache-set! x y)
  (vector-set! cache x y))

(define (child-ref x)
  (vector-ref memory x))

(define (child-set! x y)
  (vector-set! memory x y))

(define (fill-memory)
  (define (loop i)
    (unless (= i memory-size)
      (vector-set! memory i i)
      (loop (+ i 1))))
  (loop 0))         

(define (fill-cache)
  (define block-number 0)
  (define memory-adress 0)
  (define (loop idx)
    (unless (= memory-adress cache-size)
      (if (zero? (modulo idx (+ block-size 1))) 
          (begin (cache-set! idx block-number)
                 (set! block-number (+ block-number 1)))
          (begin (cache-set! idx memory-adress)
                 (set! memory-adress (+ memory-adress 1))))
      (loop (+ idx 1))))
  (loop 0)) 

(fill-memory)
(fill-cache)

(define (calc-block-number memory-adress)
  (quotient memory-adress block-size))

(define (calc-set-number block-number)
  (quotient (modulo block-number max-blocks) set-size))

(define (calc-set-adress set-number)
  (* set-number set-size (+ 1 block-size))) ;1 element per block wordt gereserveerd om het block adress op te slaan

(define (calc-block-idx set-adress set-number block-number)
  (define (loop ctr)
    (cond ((eq? ctr  set-size) #f) ;ctr staat op einde van set, block niet gevonden
          ((eq? (cache-ref (+ set-adress (* ctr (+  block-size 1)))) block-number) (+ (* set-size set-number) ctr))
          (else (loop (+ ctr 1)))))
  (loop 0))

;vind het adress van een blok dat mogelijk binnen de set te vinden is
;geeft false of een adress terug
(define (calc-block-adress block-idx)
  (if block-idx
      (* block-idx (+ block-size 1))
      false))


(define (get-cache-block set-adress block-number)
  (define (loop ctr)
    (cond ((eq? ctr set-size) #f) ;ctr staat op einde van set, block niet gevonden
          ((eq? (cache-ref (+ set-adress (* ctr (+ block-size 1)))) block-number) (+ set-adress ctr 1))
          (else (loop (+ ctr 1)))))
  (loop 0))

(define (replace-block eviction-block-number eviction-block-adress block-number adress-to-read index-in-block)
  (define (overwrite-block)
    (define write-adress (+ eviction-block-adress 1))
    (define read-adress (* block-number block-size))
    (define (loop idx)
      (unless (= idx block-size)
        (cache-set! (+ write-adress idx)(child-ref (+ read-adress idx)))
        (loop (+ idx 1))))
    (loop 0))
  (before-eviction eviction-block-number)
  (print-block eviction-block-adress)
  (cache-set! eviction-block-adress block-number)
  (overwrite-block)
  (print-block eviction-block-adress))

;from user
(define (calc-eviction-block set-number)
  0)

(define (before-eviction x)
  'done)

;testing
(define (print-block a)
  (display "block: ")
  (display (cache-ref a))(display " ")
  (display (cache-ref (+ 1 a)))(display " ")
  (display (cache-ref (+ 2 a)))(display " ")
  (display (cache-ref (+ 3 a)))(display " ")
  (display (cache-ref (+ 4 a)))(display " ")
  (display (cache-ref (+ 5 a)))(display " ")
  (newline))
(define (print-cache)
  (define cache-block-count max-blocks)
  (define (loop x)
    (unless (eq? x cache-block-count)
      (print-block (* x (+ block-size 1)))
      (loop (+ x 1))))
  (loop 0))


(define (cache-read memory-adress)
  (define index-in-block (modulo memory-adress block-size))
  (define block-number (calc-block-number memory-adress));=> blok 5
  (define set-number (calc-set-number block-number))     ;=> set 2
  (define set-adress (calc-set-adress set-number))       ;=> adress 72
  (define block-adress (get-cache-block set-adress block-number))
  (define block-index (calc-block-idx set-adress set-number block-number))
  (define block-adress2 (calc-block-adress block-index))
  (display block-adress)(display " ")(display block-adress2)(newline)
  (if block-adress
      ;cache hit
      ;block-adress is adress van eerste waarde binnen blok
      ;om eigenlijk waarde te lezen kijk op block-adress + index binnen block 
      (cache-ref (+ block-adress (modulo memory-adress block-size)))
      ;cache miss
      (begin (let* ((eviction-block-idx-in-set (calc-eviction-block set-number))
                    (eviction-block-adress (+ set-adress (* eviction-block-idx-in-set (+ block-size 1)))))
               (replace-block (+ eviction-block-idx-in-set (* set-number set-size))
                              eviction-block-adress
                              block-number
                              memory-adress
                              index-in-block)
               (cache-ref (+ eviction-block-adress index-in-block 1))))))

(define (cache-write! memory-adress new-value) 
  (define index-in-block (modulo memory-adress block-size))
  (define block-number (calc-block-number memory-adress));=> blok 5
  (define set-number (calc-set-number block-number))     ;=> set 2
  (define set-adress (calc-set-adress set-number))       ;=> adress 72
  (define block-adress (get-cache-block set-adress block-number))
  ;(display block-number)(newline)(display set-number)(newline)(display set-adress)(newline)(display block-adress)
  (if block-adress
      ;cache hit
      ;block-adress is adress van eerste waarde binnen blok
      ;om eigenlijk waarde te lezen kijk op block-adress + index binnen block 
      (cache-set! (+ block-adress (modulo memory-adress block-size)) new-value)
      ;cache miss
      (child-set! memory-adress new-value)))

