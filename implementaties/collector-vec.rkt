#lang vector/collector2

(define free-tag 'free)
(define flat-tag 'flat)
(define pair-first-pointer-tag 'plp)
(define pair-rest-pointer-tag 'prp)
(define pair-double-pointer-tag 'pdp)
(define pair-no-pointer-tag 'pnp)
(define vector-no-pointer-tag 'vnp)
(define vector-all-pointer-tag 'vap)

(print-only-errors #t)
(define next-free 0)
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
        (else (error 'gc:first "attempted to take first from a non pair, loc ~s" pr-ptr))))

(define (gc:rest pr-ptr)
  (cond ((rest-flat? pr-ptr)
         (rest-index pr-ptr))
        ((rest-pointer? pr-ptr)
         (heap-ref (rest-index pr-ptr)))
        (else (error 'gc:first "attempted to take rest from a non pair, loc ~s" pr-ptr))))

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
        (else (error 'gc:first "attempted to set first from a non pair, loc ~s" pr-ptr))))

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
        (else (error 'gc:first "attempted to set rest from a non pair, loc ~s" pr-ptr))))

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
    (error 'vector-check-size (string-append "attempted to " operation " a non-vector, loc ~s") vector-ptr))
  (unless (< idx (heap-ref (vector-size-idx vector-ptr)))
    (error 'vector-check-size (string-append "attempted to " operation " out of vector bounds, loc ~s") vector-ptr)))

(define (fill-vector start-adress end-adress reader)
  (unless (equal? start-adress end-adress)
    (heap-set! start-adress (reader start-adress))
    (fill-vector (+ 1 start-adress) end-adress reader)))

(define (empty-vector start-adress end-adress writer)
  (unless (equal? start-adress end-adress)
    (heap-set! start-adress (writer (heap-ref start-adress)))
    (empty-vector (+ 1 start-adress) end-adress writer)))

(define (vector-pointer-count++ vec-idx ignore)
  (let ((pointer-count (heap-ref (vector-pointer-count-idx vec-idx)))
        (start-adress (vector-idx vec-idx 0)))
    (heap-set! (vector-pointer-count-idx vec-idx)
               (+ pointer-count 1))
    (when (zero? pointer-count)
      (let ((writer (lambda (i)(gc:alloc-flat i))))
        (empty-vector start-adress ignore writer)
        (empty-vector (+ ignore 1)(+ start-adress (heap-ref (vector-size-idx vec-idx))) writer)
        (heap-set! vec-idx vector-all-pointer-tag)))))

(define (vector-pointer-count-- vec-idx ignore new)
  (let ((pointer-count (heap-ref (vector-pointer-count-idx vec-idx)))
        (start-adress (+ (vector-idx vec-idx 0))))
    (heap-set! (vector-pointer-count-idx vec-idx)
               (- (heap-ref (vector-pointer-count-idx vec-idx)) 1))
    (if (equal? 1 pointer-count)
        (let ((reader (lambda (i)
                        (gc:deref (heap-ref i)))))
          (fill-vector start-adress ignore reader)
          (fill-vector (+ ignore 1)(+ start-adress (heap-ref (vector-size-idx vec-idx))) reader)
          (heap-set! vec-idx vector-no-pointer-tag)
          (heap-set! ignore (gc:deref new)))
        (heap-set! ignore new))))

(define (gc:vector? loc)
  (define tag (heap-ref loc))
  (or (equal? tag vector-no-pointer-tag)
      (equal? tag vector-all-pointer-tag)))

(define (gc:make-vector size-adress init)
  (define size (gc:deref size-adress))
  (let ([ptr (alloc (+ size vector-overhead)(λ () (get-root-set init)))]
        [flat? (gc:flat? init)])
    (heap-set! ptr (if flat? vector-no-pointer-tag vector-all-pointer-tag))
    (heap-set! (vector-size-idx ptr) size)
    (heap-set! (vector-pointer-count-idx ptr) 0)
    (when flat? (set! init (gc:deref init)))
    (fill-vector (+ ptr vector-overhead) (+ ptr vector-overhead size) (lambda (i) init))
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
                     (begin (vector-pointer-count++ pr-ptr write-adress)
                            new)))
      (let ((old-flat? (gc:flat? (heap-ref write-adress)))
            (new-flat? (gc:flat? new)))
        (cond ((and old-flat? new-flat?)
               (heap-set! write-adress new))
              ((and old-flat? (not new-flat?))
               (vector-pointer-count++ pr-ptr write-adress)
               (heap-set! write-adress new))
              (new-flat?
               (vector-pointer-count-- pr-ptr write-adress new))
              (else (heap-set! write-adress new))))))

(define (gc:vector-size pr-ptr)
  (vector-check-size pr-ptr 0 "vector-size")
  (heap-ref (vector-size-idx pr-ptr)))

;garbage collection
(define forward-tag 'forward)
(define (forward-adress-index i)(+ i 1))
(define (forward? adress)
  (equal? (heap-ref adress) forward-tag))

(define (find-free-space size)
  (if (< (+ size next-free) (heap-size))
      (let ((adress next-free))
        (set! next-free (+ next-free size))
        adress)
      #f))

(define (alloc n get-roots)
  (let ([next (find-free-space n)]) ;find-free-space geeft begin adres terug of false
    (unless next ;als ik er geen gevonden heb doe een garbage-collectie
      (collect-garbage get-roots)
      (set! next (find-free-space n))) ;probeer na gc opnieuw plaats te reserveren.
    (if next      
        next
        (error 'alloc "out of space")))) ;zelf na gc niet genoeg plaats -> memory overflow

(define (collect-garbage get-roots)
  (let ((one-time-memory (make-vector (heap-size) free-tag)))
    (define (scan adress) ;scan door het tijdelijke geheugen en kopieer alle belangrijke data in het nieuwe geheugen
      (when (< adress next-free)
        (let ((tag (vector-ref one-time-memory adress)))
          (cond 
            [(equal? tag flat-tag) 
             (scan (+ adress flat-size))]
            [(equal? tag pair-first-pointer-tag) 
             (let ([old-first (vector-ref one-time-memory (first-index adress))])
               (vector-set! one-time-memory (first-index adress) (move old-first))
               (scan (+ adress pair-size)))]
            [(equal? tag pair-rest-pointer-tag) 
             (let ((old-rest (vector-ref one-time-memory (rest-index adress))))
               (vector-set! one-time-memory (rest-index adress) (move old-rest))
               (scan (+ adress pair-size)))]
            [(equal? tag pair-double-pointer-tag)
             (let ([old-first (vector-ref one-time-memory (first-index adress))]
                   [old-rest (vector-ref one-time-memory (rest-index adress))])
               (vector-set! one-time-memory (first-index adress) (move old-first))
               (vector-set! one-time-memory (rest-index adress) (move old-rest))
               (scan (+ adress pair-size)))]
            [(equal? tag pair-no-pointer-tag) 
             (scan (+ adress pair-size))]
            [(equal? tag vector-all-pointer-tag)
             (let ((size (vector-ref one-time-memory (vector-size-idx adress))))
               (do ((index 0 (+ index 1)))
                 ((= index size) 'done)
                 (vector-set! one-time-memory (vector-idx adress index)
                              (move (vector-ref one-time-memory (vector-idx adress index)))))
               (scan (+ adress size vector-overhead)))]
            [(equal? tag vector-no-pointer-tag)
             (scan (+ adress (vector-ref one-time-memory (vector-size-idx adress)) vector-overhead))]
            (else
             (error 'gc-scan "unknown tag ~s, loc ~s" tag adress))))))
    (define (move adress) ;haal adress x uit het echte geheugen
      (define (mark-position new-adress old-adress size)
        (heap-set! old-adress forward-tag) ;zet forward-tag
        (heap-set! (forward-adress-index old-adress) new-adress) ;zet forward-adress
        (set! next-free (+ next-free size))
        new-adress)
      (cond 
        ((forward? adress)
         (heap-ref (forward-adress-index adress)))
        ((gc:vector? adress)
         (let ((size (heap-ref (vector-size-idx adress))))
           (vector-set! one-time-memory next-free (heap-ref adress)) ;kopieer tag
           (vector-set! one-time-memory (vector-size-idx next-free) size) ;kopieer size
           (vector-set! one-time-memory (vector-pointer-count-idx next-free) 
                        (heap-ref (vector-pointer-count-idx adress))) ;kopieer aantal pointers
           (do ((index 0 (+ index 1)))
             ((= index size) 'done)
             (vector-set! one-time-memory (vector-idx next-free index)(heap-ref (vector-idx adress index))))
           (mark-position next-free adress (+ vector-overhead size)))) 
        ((gc:cons? adress)
         (vector-set! one-time-memory next-free (heap-ref adress)) ;kopieer tag
         (vector-set! one-time-memory (first-index next-free) (heap-ref (first-index adress))) ;kopieer first
         (vector-set! one-time-memory (rest-index next-free) (heap-ref (rest-index adress))) ;kopieer rest
         (mark-position next-free adress pair-size))         
        ((gc:flat? adress)
         (let ((value (heap-ref (flat-index adress))))
           (vector-set! one-time-memory next-free flat-tag) ;kopieer tag
           (vector-set! one-time-memory (flat-index next-free) value) ;kopieer value
           (when (procedure? value)
             (let ([roots (map read-root (procedure-roots value))])
               (for-each (lambda (root)(set-root! root (move (read-root root))))roots)))
           (mark-position next-free adress flat-size)))
        (else 
         (error 'gc:move "unknown tag ~s\n" (heap-ref adress)))))
    (display (current-heap))(newline)
    (heap-set! 196 'do)
    (heap-set! 198 'doo)
    (set! next-free 0)
    (for-each (lambda (root)(set-root! root (move (read-root root))))(get-roots))
    (scan 0)
    (do ((index 0 (+ index 1)))
      ((= index (heap-size)))
      (heap-set! index (vector-ref one-time-memory index)))
    (display (current-heap))(newline)
    ))
