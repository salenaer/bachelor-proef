#lang cache
(define cache:cache-size 5)
(define cache:set-size 3)
(define cache:block-size 5)

(define flags (make-vector (* cache:cache-size cache:set-size) false))

(define (find-false set-number error)
  (define (loop idx end)
    (cond ((= idx end) (if error (error "entire set is true") #f))
          ((not (vector-ref flags idx)) (modulo idx cache:set-size))
          (else (loop (+ idx 1) end))))
  (loop (* set-number cache:set-size)(* (+ 1 set-number) cache:set-size)))

(define (unset-all set-number)
  ;(display "unset-all: ")(display set-number)(newline)
  (define (loop idx end)
    (unless (= idx end)
          (vector-set! flags idx #f)
          (loop (+ idx 1) end)))
  (loop (* set-number cache:set-size)(* (+ 1 set-number) cache:set-size)))

(define (set-flag idx set-number)
  ;(display "set-flag ")(display set-number)(newline)
  (unless (find-false set-number #f)
    (unset-all set-number)
    (vector-set! flags idx #t)))

(define (cache:after-operation block-idx)
  (set-flag (modulo block-idx (* cache:set-size cache:cache-size)) (modulo cache:cache-size (quotient block-idx cache:set-size))))

;wat te doen voor een blok wordt overschreven in een bepaalde set => write through wordt gebruikt dus niets. 
(define (cache:before-eviction block-idx block-number source-adress)
  (void))

;wat te doen na write => write-through dus ook in main memory aanpassen
(define (cache:after-write location value block-idx)
  (child-write! location value))

(define (cache:calc-eviction-idx set-number)
   (find-false set-number #t))

(define (test-NMRU)
 (set! flags (vector #f #t #t 
                     #t #f #f 
                     #t #t #f
                     #t #f #t
                     #f #t #f))
  (display "should return 0: ")(display (cache:calc-eviction-idx 0))(newline)
  (display "should return 1: ")(display (cache:calc-eviction-idx 1))(newline)
  (display "should return 2: ")(display (cache:calc-eviction-idx 2))(newline)
  (cache:after-operation 10)
  (display "should return 0: ")(display (cache:calc-eviction-idx 3))(newline)
  (cache:after-operation 12)
  (display "should return 2: ")(display (cache:calc-eviction-idx 4))(newline))
  
       
          