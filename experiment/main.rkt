#lang scheme

(require (for-syntax scheme)
         plai/test-harness
         experiment/exports)
(provide (except-out (all-from-out scheme) #%module-begin error)
         (rename-out
          [plai-error error])
         (rename-out 
          [experiment-module-begin #%module-begin])
         (rename-out
          [experiment:func func])
         )

(provide/contract (experiment:func (-> void)))
(define (experiment:func)
  (display (experiment:button))
  (experiment:child-function))

(define-syntax (experiment-module-begin stx)
  (syntax-case stx ()
    [(_ body ...) 
     (with-syntax ([(experiment:button)
                    (map (λ (s) (datum->syntax stx s))
                         '(experiment:button))])
       #`(#%module-begin 
          (require (for-syntax scheme))
          (provide/contract (experiment:button (-> exact-nonnegative-integer?)))
           body ...
          ))]))