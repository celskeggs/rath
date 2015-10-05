#lang racket
(require syntax/strip-context)
(require rath/lang/syntax)
(require racket/rerequire)

(provide (rename-out [~ath-read read]
                     [~ath-read-syntax read-syntax])
         get-info)

(define (~ath-read in)
  (syntax->datum
   (main in)))

(define (~ath-read-syntax src in)
  (strip-context
   #`(module main rath/lang/macros
       #,(main in))))

(require parser-tools/lex)

(define-lex-abbrevs 
  (parens (union #\( #\) #\[ #\] #\{ #\})))

(define last-cache-read 0)
(define last-cache-func (void))

(define (cache-do arg)
  (let ((now (current-seconds)))
    (unless (and (< now (+ 5 last-cache-read)) (not (void? last-cache-func)))
      (dynamic-rerequire 'rath/lang/syntax-color)
      (set! last-cache-read now)
      (set! last-cache-func (dynamic-require 'rath/lang/syntax-color 'default-lexer)))
    (last-cache-func arg)))

(define ((get-info in mod line col pos) key default)
  (case key
    [(color-lexer)
     cache-do]
    [else default]))
