#lang scheme

(provide alternate-cache set-alternate-cache!)


;;; HACK!!!
(define (alternate-cache)
  (getenv "ALTERNATE_CACHE"))
(define (set-alternate-cache! path)
  (putenv "ALTERNATE_CACHE"
          (if (path? path) (path->string path) path)))
