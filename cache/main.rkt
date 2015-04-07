#lang scheme
(require (for-syntax scheme)
         plai/datatype
         plai/test-harness
         (rename-in (except-in  plai/private/gc-core set-ui!)
                    (heap-ref core:heap-ref)
                    (heap-set! core:heap-set!))
         (prefix-in exports: cache/cache-exports)
         )

(provide (except-out (all-from-out scheme) #%module-begin error)
         (all-from-out plai/datatype)
         (rename-out
          [plai-error error])
         (except-out (all-from-out plai/test-harness))
         (all-from-out plai/private/gc-core)
         (rename-out 
          [cache-module-begin #%module-begin])
         (rename-out
          [cache-read heap-ref]
          [cache-write! heap-set!])
         set-ui!)

;basis-abstractie------------------------------------------------
;idx => index in logische vector
;block-number => eigenlijk number van blok
;bv block 0, 10, 20, 30 gaan allemaal idx 0 krijgen in een cache met max-blocks 10


(define cache (make-parameter false))
(define max-blocks (make-parameter false)) ;hoeveel blokken kunnen er maximaal in de cache

(provide cache:set-ui!)
(define (cache:set-ui! ui%)
  (display (vector-length cache))
  (set! gui (new ui% [heap-vec cache])))

;zoek zoveelste waarde op in de cache vector
(provide/contract (cache-ref (location? string? . -> . heap-value?)))
(define (cache-ref location id)
  ;(display id)(newline)
  (vector-ref cache location))

;vervang zoveelste waarde in de cache vector
(provide/contract (cache-set! (location? heap-value? . -> . void?)))
(define (cache-set! location value)
  (vector-set! cache location value)
  (when gui (send gui update-view #:location location)))

;child read, single layer cache => main memory
(provide/contract (child-read (location? . -> . heap-value?)))
(define (child-read location)
  (core:heap-ref location))

;child write, single layer cache => main memory
(provide/contract (child-write! (location? heap-value? . -> . void?)))
(define (child-write! location value)
  (core:heap-set! location value))

(provide/contract [correct-cache-size? (any/c . -> . boolean?)])
(define (correct-cache-size? v) 
  (exact-nonnegative-integer? v))

;reserveer plaats voor data op te slaan + plaats om block adressen op te slaan
(provide/contract (init-cache! (-> void?)))
(define (init-cache!)
  (set! max-blocks (* exports:cache-size exports:set-size))
  (set! cache (build-vector (* max-blocks (+ 1  exports:block-size)) (λ (ix) false))))

(provide/contract [block-header? (any/c . -> . boolean?)])
(define (block-header? v)
  (or (exact-nonnegative-integer? v)(boolean? v)))

;cache-onafhankelijke code ----------------------------------------------
;op basis van het geheugen adress om te lezen welk blok moet ik vinden
(define (calc-block-number memory-adress)
  (quotient memory-adress  exports:block-size))

;op basis van het blok nummer in welke set komt een bepaald blok terecht 
(define (calc-set-number block-number)
  (quotient (modulo block-number max-blocks)  exports:set-size))

;op basis van het set-nummer bereken het adress waarop de set begint
(define (calc-set-adress set-number)
  (* set-number  exports:set-size (+ 1  exports:block-size))) ;1 element per block wordt gereserveerd om het block adress op te slaan

;vind de index van een blok dat mogelijk binnen de set te vinden is
;geeft false of een block index terug
(define (calc-block-idx set-adress set-number block-number)
  (define (loop ctr)
    (cond ((eq? ctr exports:set-size) #f) ;ctr staat op einde van set, block niet gevonden
          ((eq? (cache-ref (+ set-adress (* ctr (+  exports:block-size 1))) "id1") block-number) (+ (* exports:set-size set-number) ctr))
          (else (loop (+ ctr 1)))))
  (loop 0))

(define (get-cache-block set-adress block-number)
  (define (loop ctr)
    (cond ((eq? ctr exports:set-size) #f) ;ctr staat op einde van set, block niet gevonden
          ((eq? (cache-ref (+ set-adress (* ctr (+ exports:block-size 1))) "id2") block-number) (+ set-adress ctr 1))
          (else (loop (+ ctr 1)))))
  (loop 0))

;vind het eerste adress van een blok waarop data te vinden is dat mogelijk binnen de set te vinden is
;geeft false of een adress terug
(define (calc-block-adress block-idx)
  (if block-idx
      (+ (* block-idx (+ exports:block-size 1)) 1)
      false))

;testing
(define (print-block a)
  (display "block: ")
  (display (cache-ref a "print"))(display " ")
  (display (cache-ref (+ 1 a)"print"))(display " ")
  (display (cache-ref (+ 2 a)"print"))(display " ")
  (display (cache-ref (+ 3 a)"print"))(display " ")
  (display (cache-ref (+ 4 a)"print"))(display " ")
  (display (cache-ref (+ 5 a)"print"))(display " ")
  (newline))
(define (print-cache)
  (define cache-block-count max-blocks)
  (define (loop x)
    (unless (eq? x cache-block-count)
      (print-block (* x (+ exports:block-size 1)))
      (loop (+ x 1))))
  (loop 0))

;vervang een blok door een ander blok
(define (replace-block eviction-block-idx eviction-block-adress block-number)
  (define (overwrite-block)
    (define write-adress (+ eviction-block-adress 1))
    (define read-adress (* block-number exports:block-size))
    (define (loop idx)
      (unless (= idx  exports:block-size)
        (cache-set! (+ write-adress idx)(child-read (+ read-adress idx)))
        (loop (+ idx 1))))
    (loop 0))
  ;(display eviction-block-idx)(display " ")(display eviction-block-adress)(newline)
  ;(print-cache)
  (exports:before-eviction eviction-block-idx (cache-ref eviction-block-adress "id3") (+ 1 eviction-block-adress))
  (cache-set! eviction-block-adress block-number)
  (overwrite-block))


(provide/contract (cache-read (location? . -> . heap-value?)))
(define (cache-read memory-adress)
  (define index-in-block (modulo memory-adress exports:block-size))
  (define block-number (calc-block-number memory-adress));=> blok 5
  (define set-number (calc-set-number block-number))     ;=> set 2
  (define set-adress (calc-set-adress set-number))       ;=> adress 72
  (define block-index (calc-block-idx set-adress set-number block-number))
  (define block-adress (calc-block-adress block-index))
  (if block-adress
      ;cache hit
      ;block-adress is adress van eerste waarde binnen blok
      ;om eigenlijk waarde te lezen kijk op block-adress + index binnen block 
      (begin
        (exports:after-operation block-index)
        (cache-ref (+ block-adress index-in-block) "id4"))
      ;cache miss
      (begin (let* ((eviction-block-idx-in-set (exports:calc-eviction-idx set-number))
                    (eviction-block-adress (+ set-adress (* eviction-block-idx-in-set (+  exports:block-size 1)))))
               (set! block-index (+ eviction-block-idx-in-set (* set-number  exports:set-size)))
               (replace-block block-index
                              eviction-block-adress
                              block-number)
               (exports:after-operation block-index)
               (cache-ref (+ eviction-block-adress index-in-block 1) "id5")))))

(provide/contract (cache-write! (location? heap-value? . -> . void?)))
(define (cache-write! memory-adress new-value) 
  (define index-in-block (modulo memory-adress exports:block-size))
  (define block-number (calc-block-number memory-adress));=> blok 5
  (define set-number (calc-set-number block-number))     ;=> set 2
  (define set-adress (calc-set-adress set-number))       ;=> adress 72
  (define block-index (calc-block-idx set-adress set-number block-number))
  (define block-adress (calc-block-adress block-index))
  (if block-adress
      ;cache hit
      ;block-adress is adress van eerste waarde binnen blok
      ;om eigenlijk waarde te lezen kijk op block-adress + index binnen block 
      (begin (cache-set! (+ block-adress index-in-block) new-value)
             (exports:after-operation block-index)
             (exports:after-write memory-adress new-value block-index))
      ;cache miss => TODO: write-allocation-strategy nu enkel no-write allocation
      (child-write! memory-adress new-value))
  )

;cache gui code -------------------------------------------------------------------------

(define (set-ui! ui%)
  (set! gui (new ui% 
                 [heap-vec cache]
                 [block-size (+ exports:block-size 1)]
                 [set-size exports:set-size])))
(define gui false)

;cache require code ---------------------------------------------------------------------
(define-syntax (cache-module-begin stx)
  (syntax-case stx ()
    [(_ body ...) 
     (with-syntax ([(cache:cache-size cache:set-size cache:block-size cache:after-operation 
                     cache:after-write cache:calc-eviction-idx cache:before-eviction)
                    (map (λ (s) (datum->syntax stx s))
                         '(cache:cache-size cache:set-size cache:block-size cache:after-operation
                                            cache:after-write cache:calc-eviction-idx cache:before-eviction))])
       #`(#%module-begin 
          (require (for-syntax scheme))
          (provide/contract (cache:cache-size exact-nonnegative-integer?))
          (provide/contract (cache:set-size exact-nonnegative-integer?))
          (provide/contract (cache:block-size exact-nonnegative-integer?))
          (provide/contract (cache:after-operation (exact-nonnegative-integer? . -> . void?)))
          (provide/contract (cache:after-write (exact-nonnegative-integer? heap-value? exact-nonnegative-integer? . -> . void?)))
          (provide/contract (cache:calc-eviction-idx (exact-nonnegative-integer? . -> . exact-nonnegative-integer?)))
          (provide/contract (cache:before-eviction (exact-nonnegative-integer? block-header? exact-nonnegative-integer? . -> . void?)))       
          body ...
          ))]))