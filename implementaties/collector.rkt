#lang cache/collector

(define free-tag 'free)
(define flat-tag 'flat)
(define pair-tag 'pair)

(print-only-errors #t)

(define (find-free-space start size)
  (cond
    [(= start (heap-size))
     #f]
    [(n-free-blocks? start size)
     start]
    [else
     (find-free-space (+ start 1) size)]))

(define (n-free-blocks? start size)
  (cond
    [(= size 0) #t]
    [(= start (heap-size)) #f]
    [else 
     (and (eq? free-tag (heap-ref start))
          (n-free-blocks? (+ start 1) (- size 1)))]))


(define (init-allocator)
  (display (heap-size))
  (for ([i (in-range 0 (heap-size))])
    (heap-set! i free-tag)))

(define (gc:deref loc) 
  (cond
    [(equal? (heap-ref loc) flat-tag)
     (heap-ref (+ loc 1))]
    [else
     (error 'gc:deref "attempted to deref a non flat value, loc ~s" loc)]))

(define (gc:first pr-ptr) 
  (if (equal? (heap-ref pr-ptr) pair-tag)
      (heap-ref (+ pr-ptr 1))
      (error 'gc:first "attempted to take first from a non pair")))

(define (gc:rest pr-ptr) 
  (if (equal? (heap-ref pr-ptr) pair-tag)
      (heap-ref (+ pr-ptr 2))
      (error 'gc:rest "attempted to take rest from a non pair")))

(define (gc:flat? loc)
  (equal? (heap-ref loc)
          flat-tag))

(define (gc:cons? loc)
  (equal? (heap-ref loc)
          pair-tag))

(define (gc:set-first! pr-ptr new) 
  (if (gc:cons? pr-ptr)
      (heap-set! (+ pr-ptr 1) new)
      (error 'gc:set-first! "attempted to set first from a non pair")))

(define (gc:set-rest! pr-ptr new) 
  (if (gc:cons? pr-ptr)
      (heap-set! (+ pr-ptr 2) new)
      (error 'gc:set-rest! "attempted to set rest from a non pair")))


(define (gc:alloc-flat fv) 
  (let ([ptr (alloc 2 (λ () 
                        (if (procedure? fv)
                            (append (procedure-roots fv)
                                    (get-root-set))
                            (get-root-set))))])
    (heap-set! ptr flat-tag)
    (heap-set! (+ ptr 1) fv)
    ptr))

(define (gc:cons first rest)
  (let ([ptr (alloc 3 (λ () (get-root-set first rest)))])
    (heap-set! ptr pair-tag)
    (heap-set! (+ ptr 1) first)
    (heap-set! (+ ptr 2) rest)
    ptr))

(define (alloc n get-roots)
  (let ([next (find-free-space 0 n)]) ;find-free-space geeft begin adres terug of false
    (unless next ;als ik er geen gevonden heb doe een garbage-collectie
        (collect-garbage get-roots)
        (set! next (find-free-space 0 n))) ;probeer na gc opnieuw plaats te reserveren.
    (if next      
        next
        (error 'alloc "out of space")))) ;zelf na gc niet genoeg plaats -> memory overflow

(define (collect-garbage get-roots)
  (let ([roots (map read-root (get-roots))])
    (collect-garbage-help roots
                          (remove* roots (get-all-records 0)))))

;gray zijn alle headers (velden met een tag in) die ik al kan bereiken vanuit een root = geen garbage
;white zijn alle headers die ik nog niet heb bereikt vanuit een root = potentieël garbage
(define (collect-garbage-help gray-list white-list)
  (cond
    [(null? gray-list)(free! white-list)]
    [else
     (case (heap-ref (car gray-list))
       [(flat) 
        (let ([proc (heap-ref (+ (car gray-list) 1))])
          (if (procedure? proc)
              (let ([new-locs (map read-root (procedure-roots proc))])
                (collect-garbage-help 
                 (add-in new-locs (cdr gray-list) white-list) ;haalt new-locs uit white list en geeft gray-list terug met new-locs in. 
                 (remove* new-locs white-list)))
              (collect-garbage-help (cdr gray-list) white-list)))]
       [(pair) 
        (let ([first (heap-ref (+ (car gray-list) 1))]
              [rest (heap-ref (+ (car gray-list) 2))])
          (collect-garbage-help 
           (add-in (list first rest) (cdr gray-list) white-list)
           (remove rest (remove first white-list))))]
       [else
        (error 'collect-garbage "unknown tag ~s, loc ~s" (heap-ref (car gray-list)) (car gray-list))])]))

(define (free! white-list)
  (cond
    [(null? white-list) (void)]
    [else
     (let ([white (car white-list)])
       (case (heap-ref white)
         [(pair) 
          (heap-set! white free-tag)
          (heap-set! (+ white 1) free-tag)
          (heap-set! (+ white 2) free-tag)]
         [(flat)
          (heap-set! white free-tag)
          (heap-set! (+ white 1) free-tag)]
         [else 
          (error 'free! "unknown tag ~s\n" (heap-ref white))])
       (free! (cdr white-list)))]))

;; add-in : (listof location) (listof location) (listof location) -> (listof location)
;; computes a new set of gray addresses by addding all white elements of locs to gray
(define (add-in locs gray white)
  (cond
    [(null? locs) gray]
    [else
     (let* ([loc (car locs)]
            [white? (member loc white)])
       (add-in (cdr locs)
               (if white? (cons loc gray) gray)
               white))]))

(define (get-all-records i)
  (cond
    [(< i (heap-size))
     (case (heap-ref i)
       [(pair) (cons i (get-all-records (+ i 3)))]
       [(flat) (cons i (get-all-records (+ i 2)))]
       [(free) (get-all-records (+ i 1))]
       [else (get-all-records (+ i 1))])]
    [else null]))