#lang racket
(require syntax/strip-context)
(require "main.rkt")

(provide (rename-out [~ath-read read]
                     [~ath-read-syntax read-syntax]))

(define (~ath-read in)
  (syntax->datum
   (main in)))
(define (~ath-read-syntax src in)
  (strip-context
   #`(module main "athlang.rkt"
       #,(main in))))
