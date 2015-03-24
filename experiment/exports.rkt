#lang racket
(provide (all-defined-out))

(define experiment:button false)
(define experiment:child-function false)

(define (set-experiment:button! proc)
  (set! experiment:button proc))

(define (set-experiment:child-function! proc)
  (set! experiment:child-function proc))