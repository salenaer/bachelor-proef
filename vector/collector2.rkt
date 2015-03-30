#lang scheme

(require (for-syntax scheme)
         plai/datatype
         plai/test-harness
         plai/private/gc-core)

(provide (except-out (all-from-out scheme) #%module-begin error)
         (all-from-out plai/private/gc-core)
         (all-from-out plai/datatype)
         (rename-out
          [plai-error error])
         (except-out (all-from-out plai/test-harness))
         (rename-out 
          [collector-module-begin #%module-begin]))

(provide with-heap)
(define-syntax-rule (with-heap heap exp ...) (with-heap/proc heap (λ () exp ...)))
(define (with-heap/proc vec h)
  (unless (vector? vec)
    (error 'with-heap "expected a vector as first argument, got ~e" vec))
  (for ([v (in-vector vec)]
        [i (in-naturals)])
    (unless (heap-value? v)
      (error 'with-heap "expected the heap to contain only heap values, but found ~e at position ~a"
             v i)))
  (parameterize ([current-heap vec])
    (h)))

;;; Since we explicitly identify the procedures to be exported here, an error is raised in the
;;; collector if a procedure is not defined.
(define-syntax (collector-module-begin stx)
  (syntax-case stx ()
    [(_ body ...) 
     (with-syntax ([(init-allocator gc:deref gc:alloc-flat gc:cons gc:make-vector gc:first gc:rest gc:flat?
                                    gc:cons? gc:vector? gc:set-first! gc:set-rest! gc:vector-set!)
                    (map (λ (s) (datum->syntax stx s))
                         '(init-allocator gc:deref gc:alloc-flat gc:cons gc:make-vector gc:first gc:rest gc:flat? 
                                          gc:cons? gc:vector? gc:set-first! gc:set-rest! gc:vector-set!))])
       #`(#%module-begin 
          
          (require (for-syntax scheme))
          
          (provide/contract (init-allocator (-> any)))
          
          (provide/contract (gc:deref (location? . -> . heap-value?)))
          
          (provide/contract (gc:alloc-flat (heap-value? . -> . location?)))
          (provide/contract (gc:cons (location? location? . -> . location?)))
          (provide/contract (gc:make-vector (exact-nonnegative-integer? heap-value? . -> . location?)))
          
          (provide/contract (gc:first (location? . -> . location?)))
          (provide/contract (gc:rest (location? . -> . location?)))
          
          (provide/contract (gc:flat? (location? . -> . boolean?)))
          (provide/contract (gc:cons? (location? . -> . boolean?)))
          (provide/contract (gc:vector? (location? . -> . boolean?)))
          
          (provide/contract (gc:set-first! (location? location? . -> . void?)))
          (provide/contract (gc:set-rest! (location? location? . -> . void?)))
          (provide/contract (gc:vector-set! (location? exact-nonnegative-integer? location? . -> . void?)))
          
          
          
          body ...
          
          ))]))
