#lang cache/collector
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
  (for ((i (in-range 0 (heap-size))))
    (heap-set! i free-tag)))

;flat values
(define flat-size 2)
(define (flat-index x)
  (+ x 1))
(define (gc:flat? loc)
  (equal? (heap-ref loc)
          flat-tag))

(define (gc:alloc-flat fv)
  (let ((ptr (alloc flat-size (λ () 
                                (if (procedure? fv)
                                    (append (procedure-roots fv)
                                            (get-root-set))
                                    (get-root-set))))))
    (heap-set! ptr flat-tag)
    (heap-set! (flat-index ptr) fv)
    ptr))

(define (gc:deref loc) 
  (cond
    ((equal? (heap-ref loc) flat-tag)
     (heap-ref (flat-index loc)))
    (else
     (error 'gc:deref "attempted to deref a non flat value, loc ~s" loc))))

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
  (let* ((ptr (alloc pair-size (λ () (append (get-root-set) 
                                             (list (make-root 'first (lambda () first)(lambda (x)(set! first x)))
                                                   (make-root 'rest (lambda () rest)(lambda (x)(set! rest x))))))))
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
(define (vector-length-idx vec-idx)
  (+ vec-idx 1))
(define (vector-pointer-count-idx idx)
  (+ idx 2))

(define (vector-check-size vector-ptr idx operation)
  (unless (gc:vector? vector-ptr)
    (error 'vector-check-size (string-append "attempted to " operation " a non-vector, loc ~s") vector-ptr))
  (unless (< idx (heap-ref (vector-length-idx vector-ptr)))
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
        (empty-vector (+ ignore 1)(+ start-adress (heap-ref (vector-length-idx vec-idx))) writer)
        (heap-set! vec-idx vector-all-pointer-tag)))))

(define (vector-pointer-count-- vec-idx ignore new)
  (let ((pointer-count (heap-ref (vector-pointer-count-idx vec-idx)))
        (start-adress (vector-idx vec-idx 0)))
    (heap-set! (vector-pointer-count-idx vec-idx)
               (- (heap-ref (vector-pointer-count-idx vec-idx)) 1))
    (if (equal? 1 pointer-count)
        (let ((reader (lambda (i)
                        (gc:deref (heap-ref i)))))
          (fill-vector start-adress ignore reader)
          (fill-vector (+ ignore 1)(+ start-adress (heap-ref (vector-length-idx vec-idx))) reader)
          (heap-set! vec-idx vector-no-pointer-tag)
          (heap-set! ignore (gc:deref new)))
        (heap-set! ignore new))))

(define (gc:vector? loc)
  (define tag (heap-ref loc))
  (or (equal? tag vector-no-pointer-tag)
      (equal? tag vector-all-pointer-tag)))

(define (gc:make-vector size-adress init)
  (let* ((size (gc:deref size-adress))
         (ptr (alloc (+ size vector-overhead)(λ () (append (get-root-set) (list (make-root 'init (lambda () init)(lambda (x)(set! init x))))))))
         (flat? (gc:flat? init)))
    (if (zero? size)
        (error "allocating empty vector")
        (begin
          (heap-set! ptr (if flat? vector-no-pointer-tag vector-all-pointer-tag))
          (heap-set! (vector-length-idx ptr) size)
          (heap-set! (vector-pointer-count-idx ptr) (if flat? 0 size))
          (when flat? (set! init (gc:deref init)))
          (fill-vector (vector-idx ptr 0) (vector-idx ptr size) (lambda (i) init))
          ptr))))

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

(define (gc:vector-length pr-ptr)
  (vector-check-size pr-ptr 0 "vector-length")
  (gc:alloc-flat (heap-ref (vector-length-idx pr-ptr))))

;garbage collection
(define (find-free-space start size)
  (cond
    ((= start (heap-size))
     #f)
    ((n-free-blocks? start size)
     start)
    (else
     (find-free-space (+ start 1) size))))

(define (n-free-blocks? start size)
  (cond
    ((= size 0) #t)
    ((= start (heap-size)) #f)
    (else 
     (and (eq? free-tag (heap-ref start))
          (n-free-blocks? (+ start 1) (- size 1))))))

(define (alloc n get-roots)
  (let ((next (find-free-space 0 n))) ;find-free-space geeft begin adres terug of false
    (unless next ;als ik er geen gevonden heb doe een garbage-collectie
      (collect-garbage get-roots)
      (set! next (find-free-space 0 n))) ;probeer na gc opnieuw plaats te reserveren.
    (if next      
        next
        (error 'alloc "out of space")))) ;zelf na gc niet genoeg plaats -> memory overflow

(define (collect-garbage get-roots)
  (let ((roots (map read-root (get-roots)))
        (stats (return-stats)))
    ;(display "roots: ")(newline)
    ;(for-each (lambda (root)(display root)(display " ")(display (read-root root))(newline))(get-root-set))(newline)
    (collect-garbage-help roots
                          (remove* roots (get-all-records 0)))
    (set-stats! stats)))

;gray zijn alle headers (velden met een tag in) die ik al kan bereiken vanuit een root = geen garbage
;white zijn alle headers die ik nog niet heb bereikt vanuit een root = potentieël garbage
(define (collect-garbage-help gray-list white-list)
  ;(display gray-list)(newline)
  (cond
    ((null? gray-list)(free! white-list))
    (else
     (case (heap-ref (car gray-list))
       ((flat) 
        (let ((proc (heap-ref (+ (car gray-list) 1))))
          (if (procedure? proc)
              (let ((new-locs (map read-root (procedure-roots proc))))
                (collect-garbage-help 
                 (add-in new-locs (cdr gray-list) white-list) ;haalt new-locs uit white list en geeft gray-list terug met new-locs in. 
                 (remove* new-locs white-list)))
              (collect-garbage-help (cdr gray-list) white-list))))
       ((plp) 
        (let ((first (heap-ref (+ (car gray-list) 1))))
          (collect-garbage-help 
           (add-in (list first) (cdr gray-list) white-list)
           (remove first white-list))))
       ((prp) 
        (let ((rest (heap-ref (+ (car gray-list) 2))))
          (collect-garbage-help 
           (add-in (list rest) (cdr gray-list) white-list)
           (remove rest white-list))))
       ((pdp) 
        (let ((first (heap-ref (+ (car gray-list) 1)))
              (rest (heap-ref (+ (car gray-list) 2))))
          (collect-garbage-help 
           (add-in (list first rest) (cdr gray-list) white-list)
           (remove rest (remove first white-list)))))
       ((pnp)
        (collect-garbage-help (cdr gray-list) white-list))
       ((vnp)
        (collect-garbage-help (cdr gray-list) white-list))
       ((vap)
        (let ((vector-elems (list-all-from-vector (car gray-list) '() (heap-ref (vector-length-idx (car gray-list))) 0)))
          (collect-garbage-help
           (add-in vector-elems (cdr gray-list) white-list)
           (remove* vector-elems white-list))))
       (else
        (error 'collect-garbage "unknown tag ~s, loc ~s" (heap-ref (car gray-list)) (car gray-list)))))))

(define (free! white-list)
  (cond
    ((null? white-list) (void))
    (else
     (let ((white (car white-list)))
       (cond
         ((gc:vector? white)
          (fill-vector (vector-idx white 0) (vector-idx white (heap-ref (vector-length-idx white))) (lambda (i) free-tag))
          (heap-set! white free-tag)
          (heap-set! (vector-length-idx white) free-tag)
          (heap-set! (vector-pointer-count-idx white) free-tag))      
         ((gc:cons? white) 
          (heap-set! white free-tag)
          (heap-set! (+ white 1) free-tag)
          (heap-set! (+ white 2) free-tag))
         ((gc:flat? white)
          (heap-set! white free-tag)
          (heap-set! (+ white 1) free-tag))
         (else 
          (error 'free! "unknown tag ~s\n" (heap-ref white))))
       (free! (cdr white-list))))))

(define (list-all-from-vector vector list size ctr)
  (if (= ctr size)
      list
      (list-all-from-vector 
       vector 
       (cons 
        (heap-ref (vector-idx vector ctr))
        list)
       size
       (+ ctr 1))))

;; add-in : (listof location) (listof location) (listof location) -> (listof location)
;; computes a new set of gray addresses by addding all white elements of locs to gray
(define (add-in locs gray white)
  (cond
    ((null? locs) gray)
    (else
     (let* ((loc (car locs))
            (white? (member loc white)))
       (add-in (cdr locs)
               (if white? (cons loc gray) gray)
               white)))))

(define (get-all-records i)
  (if (< i (heap-size))
      (cond 
        ((gc:vector? i)(cons i (get-all-records (+ i (heap-ref (vector-length-idx i)) vector-overhead))))
        ((gc:cons? i) (cons i (get-all-records (+ i pair-size))))
        ((gc:flat? i) (cons i (get-all-records (+ i flat-size))))
        ((equal? free-tag (heap-ref i)) (get-all-records (+ i 1)))
        (else (error 'free! "unknown tag ~s, loc ~s" (heap-ref i) i)))
      null))
