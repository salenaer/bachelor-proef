#lang cache
(require data/queue)
(define cache:cache-size 5)
(define cache:set-size 3)
(define cache:block-size 5)
(define cache:write-allocation false)

(define block-count (* cache:cache-size cache:set-size))

(define queues (make-vector cache:cache-size false)) ;maak een queue per set
(define available (make-vector block-count false))   ;ik moet weten wanneer ik een block-idx in de queue moet zetten

(define (fill-queue queue ctr)
  (if (= ctr cache:set-size)
      queue
      (begin (enqueue! queue ctr)
             (fill-queue queue (+ ctr 1)))
      ))
(define (create-queues ctr)
  (unless (= ctr cache:cache-size)
    (vector-set! queues ctr (fill-queue (make-queue) 0))
    (create-queues (+ ctr 1))))

(define (print-queues)
  (define (loop ctr)
    (unless (= ctr cache:cache-size)
      (display (queue->list (vector-ref queues ctr)))(newline)
      (loop (+ ctr 1))))
  (display "queues:")(newline)
  (loop 0)
  (newline))

(create-queues 0)

(define (cache:after-operation block-idx)
  (unless (vector-ref available block-idx) ;als de block al in de queue zit moet ik niets doen
    (enqueue! (vector-ref queues (quotient block-idx cache:set-size)) (modulo block-idx cache:set-size))
    (vector-set! available block-idx #t)))

;wat te doen voor een blok wordt overschreven in een bepaalde set => write through wordt gebruikt dus niets. 
(define (cache:before-eviction block-idx block-number source-adress)
  (vector-set! available block-idx #f))

;wat te doen na write => write-through dus ook in main memory aanpassen
(define (cache:after-write location value block-idx)
  (child-write! location value))

(define (cache:calc-eviction-idx set-number)
  (dequeue! (vector-ref queues set-number)))


