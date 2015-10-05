#lang racket
(require parser-tools/lex)

(provide default-lexer)

#| (define default-lexer
  (lexer
   (any-char
    (values "" 'comment #f (position-offset start-pos) (position-offset end-pos)))
   ((eof)
    (values lexeme 'eof #f #f #f)))) |#

(define default-lexer
  (lexer
   [(repetition 1 +inf.0 numeric)
    (values lexeme 'constant #f (position-offset start-pos) (position-offset end-pos))]
   [(union "import" "~ATH" "EXECUTE" "NULL" "TRUE" "FALSE" (char-set ";,=&|^<>!+-*/."))
    (values lexeme 'hash-color-keyword #f (position-offset start-pos) (position-offset end-pos))]
   [(char-set "(){}[]")
    (values lexeme 'hash-color-keyword (string->symbol lexeme) (position-offset start-pos) (position-offset end-pos))]
   [(repetition 1 +inf.0 (union alphabetic (char-set "_~")))
    (values lexeme 'symbol #f (position-offset start-pos) (position-offset end-pos))]
   [(concatenation #\\ any-char)
    (values lexeme 'constant #f (position-offset start-pos) (position-offset end-pos))]
   [(concatenation #\" (repetition 0 +inf.0 (union (char-complement (char-set "\"\\")) (concatenation #\\ any-char))) #\")
    (values lexeme 'constant #f (position-offset start-pos) (position-offset end-pos))]
   [(concatenation "//" (repetition 0 +inf.0 (char-complement #\newline)) #\newline)
    (values lexeme 'comment #f (position-offset start-pos) (position-offset end-pos))]
   [whitespace
    (values lexeme 'whitespace #f (position-offset start-pos) (position-offset end-pos))]
   [(eof)
    (values lexeme 'eof #f #f #f)]))
