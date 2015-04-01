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
    (display "filling vector ")(display start-adress)(display " ")(display end-adress)(newline)
    (heap-set! start-adress (reader))
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
      (empty-vector start-adress ignore (lambda (i)(gc:alloc-flat i)))
      (empty-vector (+ ignore 1)(+ start-adress (heap-ref (vector-size-idx vec-idx))) (lambda (i)(gc:alloc-flat i)))
      (heap-set! vec-idx vector-all-pointer-tag))))

(define (vector-pointer-count-- vec-idx ignore)
  (let ((pointer-count (heap-ref (vector-pointer-count-idx vec-idx)))
        (start-adress (+ (vector-idx vec-idx 0))))
  (heap-set! (vector-pointer-count-idx vec-idx)
             (- (heap-ref (vector-pointer-count-idx vec-idx)) 1))
    (when (equal? 1 pointer-count)
      (let* ((i start-adress)
            (reader (lambda ()
                    (set! i (+ i 1))
                    (gc:deref (heap-ref (- i 1))))))
      (fill-vector start-adress ignore reader)
      (set! i (+ i 1))
      (fill-vector (+ ignore 1)(+ start-adress (heap-ref (vector-size-idx vec-idx))) reader)
      (heap-set! vec-idx vector-no-pointer-tag)))))

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
                     (begin (vector-pointer-count++ pr-ptr write-adress)
                            new)))
      (let ((old-flat? (gc:flat? write-adress))
            (new-flat? (gc:flat? new)))
        (cond ((and old-flat? new-flat?)
               (heap-set! write-adress (gc:deref new)))
              ((and old-flat? (not new-flat?))
               (vector-pointer-count++ pr-ptr write-adress)
               (heap-set! write-adress new))
              (new-flat?
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
      (display "collecting garbage")(newline)
      (collect-garbage get-roots)
      (set! next (find-free-space 0 n))) ;probeer na gc opnieuw plaats te reserveren.
    (if next      
        next
        (error 'alloc "out of space")))) ;zelf na gc niet genoeg plaats -> memory overflow

(define (collect-garbage get-roots)
  (let ([roots (map read-root (get-roots))])
    (collect-garbage-help roots
                          (remove* roots (get-all-records 0 '())))))

;gray zijn alle headers (velden met een tag in) die ik al kan bereiken vanuit een root = geen garbage
;white zijn alle headers die ik nog niet heb bereikt vanuit een root = potentieël garbage
;(define (collect-garbage-help gray-list white-list)
;  (cond
;    [(null? gray-list)(free! white-list)]
;    [else
;     (case (heap-ref (car gray-list))
;       [(flat) 
;        (let ([proc (heap-ref (gc:deref (car gray-list)))])
;          (if (procedure? proc)
;              (let ([new-locs (map read-root (procedure-roots proc))])
;                (collect-garbage-help 
;                 (add-in new-locs (cdr gray-list) white-list) ;haalt new-locs uit white list en geeft gray-list terug met new-locs in. 
;                 (remove* new-locs white-list)))
;              (collect-garbage-help (cdr gray-list) white-list)))]
;       [(pair-first-pointer-tag) 
;        (let ([first (heap-ref (first-index (car gray-list)))])
;          (collect-garbage-help 
;           (add-in (list first) (cdr gray-list) white-list)
;           (remove first white-list)))]
;       [(pair-rest-pointer-tag) 
;        (let ([rest (heap-ref (rest-index (car gray-list)))])
;          (collect-garbage-help 
;           (add-in (list rest) (cdr gray-list) white-list)
;           (remove rest white-list)))]
;       [(pair-double-pointer-tag)
;        (let ([first (heap-ref (first-index (car gray-list)))]
;              [rest (heap-ref (rest-index (car gray-list)))])
;          (collect-garbage-help 
;           (add-in (list first rest) (cdr gray-list) white-list)
;           (remove rest (remove first white-list))))]
;       [(pair-no-pointer-tag) 
;        (collect-garbage-help 
;         (cdr gray-list)
;         white-list)]
;       [(vector-no-pointer-tag)
;        (collect-garbage-help 
;         (cdr gray-list)
;         white-list)]
;       [(vector-all-pointer-tag)
;        (let ([pointers (get-all-vector-pointers (car gray-list)(heap-ref (vector-size-idx (car gray-list))) 0 '())])
;          (collect-garbage-help 
;           (add-in pointers (cdr gray-list) white-list)
;           (remove-list pointers white-list)))]
;       [else
;        (error 'collect-garbage "unknown tag ~s, loc ~s" (heap-ref (car gray-list)) (car gray-list))])]))

(define (collect-garbage-help gray-list white-list)
  (cond
    [(null? gray-list)(free! white-list)]
    [else
     (let ((tag (heap-ref (car gray-list))))
       (cond 
         [(equal? tag flat-tag) 
          (let ([proc (heap-ref (gc:deref (car gray-list)))])
            (if (procedure? proc)
                (let ([new-locs (map read-root (procedure-roots proc))])
                  (collect-garbage-help 
                   (add-in new-locs (cdr gray-list) white-list) ;haalt new-locs uit white list en geeft gray-list terug met new-locs in. 
                   (remove* new-locs white-list)))
                (collect-garbage-help (cdr gray-list) white-list)))]
         [(equal? tag pair-first-pointer-tag) 
          (let ([first (heap-ref (first-index (car gray-list)))])
            (collect-garbage-help 
             (add-in (list first) (cdr gray-list) white-list)
             (remove first white-list)))]
         [(equal? tag pair-rest-pointer-tag) 
          (let ([rest (heap-ref (rest-index (car gray-list)))])
            (collect-garbage-help 
             (add-in (list rest) (cdr gray-list) white-list)
             (remove rest white-list)))]
         [(equal? tag pair-double-pointer-tag)
          (let ([first (heap-ref (first-index (car gray-list)))]
                [rest (heap-ref (rest-index (car gray-list)))])
            (collect-garbage-help 
             (add-in (list first rest) (cdr gray-list) white-list)
             (remove rest (remove first white-list))))]
         [(equal? tag pair-no-pointer-tag) 
          (collect-garbage-help 
           (cdr gray-list)
           white-list)]
         [(equal? tag vector-no-pointer-tag)
          (collect-garbage-help 
           (cdr gray-list)
           white-list)]
         [(equal? vector-all-pointer-tag)
          (let ([pointers (get-all-vector-pointers (car gray-list)(heap-ref (vector-size-idx (car gray-list))) 0 '())])
            (collect-garbage-help 
             (add-in pointers (cdr gray-list) white-list)
             (remove-list pointers white-list)))]
         (else
          (error 'collect-garbage "unknown tag ~s, loc ~s" (heap-ref (car gray-list)) (car gray-list)))))]))
  
  (define (get-all-vector-pointers vec end idx list)
    (if (equal? idx end)
        list
        (get-all-vector-pointers vec end (+ idx 1) (cons (heap-ref (vector-idx vec idx)) list))))
  
  (define (remove-list pointers list)
    (if (null? pointers)
        list
        (remove-list (cdr pointers)(remove (car pointers) list))))
  
  (define (free! white-list)
    (cond
      [(null? white-list) (void)]
      [else
       (let* ([white (car white-list)])
         (cond
           ((gc:vector? white)
            (fill-vector white (+ white vector-overhead (heap-ref (vector-size-idx white))) (lambda () free-tag)))          
           ((gc:cons? white) 
            (heap-set! white free-tag)
            (heap-set! (first-index white) free-tag)
            (heap-set! (rest-index white) free-tag))
           ((gc:flat? white)
            (heap-set! white free-tag)
            (heap-set! (flat-index white) free-tag))
           (else 
            (error 'free! "unknown tag ~s\n" (heap-ref white))))
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
  
  (define (get-all-records i list)
    (if (< i (heap-size))
        list
        (cond 
          ((gc:vector? i)
           (get-all-records (+ i vector-overhead (heap-ref (vector-size-idx i))) (cons i list)))         
          ((gc:cons? i) 
           (get-all-records (+ i pair-size)(cons i list)))
          ((gc:flat? i)
           (get-all-records (+ i flat-size)(cons i list)))
          ((equal? (heap-ref i) free-tag)
           (get-all-records (+ i 1) list))
          (else 
           (error 'get-all-records "unknown tag ~s\n" (heap-ref i))))))