#lang vector/collector2

(define free-tag 'free)
(define flat-tag 'flat)
(define pair-first-pointer-tag 'plp)
(define pair-rest-pointer-tag 'prp)
(define pair-double-pointer-tag 'pdp)
(define pair-no-pointer-tag 'pnp)
(define vector-tag 'vector)
(define vector-no-pointer-tag 'vnp)
(define vector-all-pointer-tag 'vap)

(print-only-errors #t)

(define (init-allocator) 
  (for ([i (in-range 0 (heap-size))])
    (heap-set! i free-tag)))

;flat values
(define flat-size 2)
(define (flat-index x)
  (+ x 1))
(define (gc:flat? loc)
  (equal? (heap-ref loc)
          flat-tag))

(define (gc:alloc-flat fv) 
  (let ([ptr (alloc flat-size (λ () 
                                (if (procedure? fv)
                                    (append (procedure-roots fv)
                                            (get-root-set))
                                    (get-root-set))))])
    (heap-set! ptr flat-tag)
    (heap-set! (flat-index ptr) fv)
    ptr))

(define (gc:deref loc) 
  (cond
    [(equal? (heap-ref loc) flat-tag)
     (heap-ref (flat-index loc))]
    [else
     (error 'gc:deref "attempted to deref a non flat value, loc ~s" loc)]))

;pair
(define pair-size 3)
(define (first-index pair-idx)
  (+ pair-idx 1))
(define (rest-index pair-idx)
  (+ pair-idx 2))

(define (first-flat? loc)
  (define tag (heap-ref loc))
  (or (equal? tag pair-no-pointer-tag)
      (equal? tag pair-rest-pointer-tag)))
(define (first-pointer? loc)
  (define tag (heap-ref loc));ik wil geen not gebruiken zodanig dat als zowel flat als adress faalt => geen cons cell
  (or (equal? tag pair-double-pointer-tag)
      (equal? tag pair-first-pointer-tag)))

(define (rest-flat? loc)
  (define tag (heap-ref loc))
  (or (equal? tag pair-no-pointer-tag)
      (equal? tag pair-first-pointer-tag)))
(define (rest-pointer? loc)
  (define tag (heap-ref loc))
  (or (equal? tag pair-double-pointer-tag)
      (equal? tag pair-rest-pointer-tag)))

(define (gc:cons? loc)
  (define tag (heap-ref loc))
  (or (equal? tag pair-no-pointer-tag)
      (equal? tag pair-first-pointer-tag)
      (equal? tag pair-rest-pointer-tag)
      (equal? tag pair-double-pointer-tag)))

(define (gc:cons first rest)
  (let ([ptr (alloc pair-size (λ () (get-root-set first rest)))]
        (tag pair-no-pointer-tag))
    (if (not (gc:flat? first))
        (begin (set! tag pair-first-pointer-tag)
               (heap-set! (first-index ptr) first))
        (heap-set! (first-index ptr) (gc:deref first)))
    (if (not (gc:flat? rest))
        (begin (set! tag (if (equal? tag pair-first-pointer-tag)
                             pair-double-pointer-tag
                             pair-rest-pointer-tag))
               (heap-set! (rest-index ptr) rest))
        (heap-set! (rest-index ptr) (gc:deref rest)))
    (heap-set! ptr tag)
    ptr))

(define (gc:first pr-ptr)
  (cond ((first-flat? pr-ptr)
         (first-index pr-ptr))
        ((first-pointer? pr-ptr)
         (heap-ref (first-index pr-ptr)))
        (else (error 'gc:first "attempted to take first from a non pair"))))

(define (gc:rest pr-ptr)
  (cond ((rest-flat? pr-ptr)
         (rest-index pr-ptr))
        ((rest-pointer? pr-ptr)
         (heap-ref (rest-index pr-ptr)))
        (else (error 'gc:first "attempted to take rest from a non pair"))))

(define (gc:set-first! pr-ptr new)
  (define tag (heap-ref pr-ptr))
  (cond ((first-flat? pr-ptr)
         (if (gc:flat? new)
             (heap-set! (first-index pr-ptr) (gc:deref new))
             (begin (heap-set! (first-index pr-ptr) new)
                    (heap-set! pr-ptr (if (equal? tag pair-no-pointer-tag)
                                          pair-first-pointer-tag
                                          pair-double-pointer-tag)))))
        ((first-pointer? pr-ptr) 
         (if (gc:flat? new)
             (begin (heap-set! (first-index pr-ptr) (gc:deref new))
                    (heap-set! pr-ptr (if (equal? tag pair-double-pointer-tag)
                                          pair-rest-pointer-tag
                                          pair-no-pointer-tag)))
             (heap-set! (first-index pr-ptr) new)))
        (else (error 'gc:first "attempted to set first from a non pair"))))

(define (gc:set-rest! pr-ptr new)
  (define tag (heap-ref pr-ptr))
  (cond ((rest-flat? pr-ptr)
         (if (gc:flat? new)
             (heap-set! (rest-index pr-ptr) (gc:deref new))
             (begin (heap-set! (rest-index pr-ptr) new)
                    (heap-set! pr-ptr (if (equal? tag pair-no-pointer-tag)
                                          pair-rest-pointer-tag
                                          pair-double-pointer-tag)))))
        ((rest-pointer? pr-ptr) 
         (if (gc:flat? new)
             (begin (heap-set! (rest-index pr-ptr) (gc:deref new))
                    (heap-set! pr-ptr (if (equal? tag pair-double-pointer-tag)
                                          pair-first-pointer-tag
                                          pair-no-pointer-tag)))
             (heap-set! (rest-index pr-ptr) new)))
        (else (error 'gc:first "attempted to set rest from a non pair"))))

;vector
(define vector-overhead 3)
(define (vector-idx vec-idx ref)
  (+ vec-idx vector-overhead ref))
(define (vector-size-idx vec-idx)
  (+ vec-idx 1))
(define (vector-pointer-count-idx idx)
  (+ idx 2))

(define (vector-check-size vector-ptr idx operation)
  (unless (gc:vector? vector-ptr)
    (error 'vector-check-size (string-append "attempted to " operation " a non-vector")))
  (unless (< idx (heap-ref (vector-size-idx vector-ptr)))
    (error 'vector-check-size (string-append "attempted to " operation " out of vector bounds"))))

(define (fill-vector start-adress end-adress reader)
  (unless (equal? start-adress end-adress)
    (heap-set! start-adress (reader))
    (fill-vector (+ 1 start-adress) end-adress reader)))

(define (vector-pointer-count vec-idx)
  (heap-ref (vector-pointer-count-idx vec-idx)))
(define (vector-pointer-count++ vec-idx)
  (heap-set! (vector-pointer-count-idx vec-idx)
             (+ (heap-ref (vector-pointer-count-idx vec-idx)) 1)))
(define (vector-pointer-count-- vec-idx ignore)
  (heap-set! (vector-pointer-count-idx vec-idx)
             (- (heap-ref (vector-pointer-count-idx vec-idx)) 1)))

(define (gc:vector? loc)
  (define tag (heap-ref loc))
  (or (equal? tag vector-no-pointer-tag)
      (equal? tag vector-all-pointer-tag)))

(define (gc:make-vector size-adress init)
  (define size (gc:deref size-adress))
  (let ([ptr (alloc (+ size vector-overhead)(λ () (get-root-set)))]
        [flat? (gc:flat? init)])
    (heap-set! ptr (if flat? vector-no-pointer-tag vector-all-pointer-tag))
    (heap-set! (vector-size-idx ptr) size)
    (heap-set! (vector-pointer-count-idx ptr) 0)
    (when flat? (set! init (gc:deref init)))
    (fill-vector (+ ptr vector-overhead) (+ ptr vector-overhead size) (lambda () init))
    ptr))

(define (gc:vector-ref pr-ptr idx)
  (define tag (heap-ref pr-ptr))
  (set! idx (gc:deref idx))
  (vector-check-size pr-ptr idx "vector-ref")
  (if (equal? tag vector-no-pointer-tag)
      (vector-idx pr-ptr idx)
      (heap-ref (vector-idx pr-ptr idx))))

(define (gc:vector-set! pr-ptr idx new)
  (define tag (heap-ref pr-ptr))
  (set! idx (gc:deref idx))
  (define write-adress (vector-idx pr-ptr idx))
  (vector-check-size pr-ptr idx "vector-set!")
  (if (equal? tag vector-no-pointer-tag)
      (heap-set! write-adress 
                   (if (gc:flat? new) 
                       (gc:deref new)
                       (begin (vector-pointer-count++ pr-ptr)
                              new)))
      (let ((old-flat? (gc:flat? write-adress))
            (new-flat? (gc:flat? new)))
        (cond ((and old-flat? new-flat?)
               (heap-set! write-adress (gc:deref new)))
              ((and old-flat? (not new-flat?))
               (vector-pointer-count++ pr-ptr)
               (heap-set! write-adress new))
              ((new-flat?)
               (vector-pointer-count-- pr-ptr write-adress)
               (heap-set! write-adress (gc:deref new)))
              (else (heap-set! write-adress new))))))

;garbage collection
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