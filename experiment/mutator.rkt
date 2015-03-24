#lang scheme
(require (for-syntax scheme)
         experiment/exports
         (only-in experiment/main func)
         scheme/stxparam)

(provide (rename-out 
          [experiment-module-begin #%module-begin])
         (except-out (all-from-out scheme) #%module-begin error)
         func)

(define-for-syntax (allocator-setup-internal stx)
  (syntax-case stx ()
    [(experiment-module)
     (with-syntax ([(experiment:button)                              
                   (map (λ (s) (datum->syntax stx s))
                        '(experiment:button))])
     (begin
       #`(begin 
           (require experiment-module)    
       (set-experiment:button! experiment:button)
       (set-experiment:child-function! (lambda ()(display 0))))))]
    [_ (raise-syntax-error 'mutator 
                           "Mutator must start with an 'allocator-setup' expression, such as: (allocator-setup <module-path> <literal-number>)"
                           stx)]))

(define-for-syntax allocator-setup-error-msg
  "Mutator must start with an 'allocator-setup' expression, such as: (allocator-setup <collector-module-path> (<cache-module-path> <literal-number>) <literal-number>)")


(define-syntax (experiment-module-begin stx) ;hier
  (syntax-case stx (allocator-setup)
    [(_ (allocator-setup . setup) module-expr ...)
     (begin
       (syntax-case #'setup ()
         [(experiment)                  ;aangepast
          (begin
            (unless (module-path? (syntax->datum #'experiment))
              (raise-syntax-error 'allocator-setup "expected a module path" #'experiment)))]
         [_
          (raise-syntax-error 'mutator allocator-setup-error-msg (syntax/loc #'setup (allocator-setup . setup)))])
       (quasisyntax/loc stx
         (#%module-begin
          #,(allocator-setup-internal #'setup)
          #,@(for/list ([me (in-list (syntax->list #'(module-expr ...)))])
               (quasisyntax/loc me
                 (mutator-top-interaction . #,me))))))]
    [(_ first-expr module-expr ...)
     (raise-syntax-error 'mutator allocator-setup-error-msg #'first-expr)]
    [(_)
     (raise-syntax-error 'mutator allocator-setup-error-msg)]))

