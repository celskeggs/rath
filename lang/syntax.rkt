#lang racket
(require rath/lang/parser)
(require ragg/support)
(require parser-tools/lex)
(require (except-in syntax/parse boolean))

(provide main)

(define (process-escapes str)
  (apply string
         (let iter ((remain (string->list str)))
           (cond ((empty? remain) empty)
                 ((eq? (car remain) #\\) (cons (cadr remain) (iter (cddr remain))))
                 (else (cons (car remain) (iter (cdr remain))))))))

(define (tokenize ip)
  (port-count-lines! ip)
  (define lexer
    (lexer-src-pos
     [(repetition 1 +inf.0 numeric)
      (token 'INTEGER (string->number lexeme))]
     [(union "import" "~ATH" "EXECUTE" "NULL" "TRUE" "FALSE" (char-set ";(){}[],=&|^<>!+-*/."))
      (token lexeme lexeme)]
     [(repetition 1 +inf.0 (union alphabetic (char-set "_~")))
      (token 'VARIABLE (string->symbol lexeme))]
     [(concatenation #\\ any-char)
      (token 'INTEGER (char->integer (string-ref lexeme 1)))]
     [(concatenation #\" (repetition 0 +inf.0 (union (char-complement (char-set "\"\\")) (concatenation #\\ any-char))) #\")
      (token 'STRING (process-escapes (substring lexeme 1 (- (string-length lexeme) 1))))]
     [(concatenation "//" (repetition 0 +inf.0 (char-complement #\newline)) #\newline)
      (token 'COMMENT lexeme #:skip? #t)]
     [whitespace
      (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof)
      (void)]))
  (define (next-token) (lexer ip))
  next-token)

(define (simplify stx)
  (define stxe (syntax-e stx))
  (cond ((string? stxe) stx)
        ((symbol? stxe) stx)
        ((number? stxe) stx)
        ((list? stxe) (simplify-call stx stxe))))

(define (simplify-call stx stxe)
  (define new-stx (datum->syntax stx (cons (car stxe)
                                           (map simplify (cdr stxe)))))
  (syntax-case new-stx (program statement statement-or-rvalue lvalue call constant boolean rvalue-exec-wrapper
                                assignment-op logic-op comparison-op bin-op add-op mul-op unary-op rvalue rvalue1 rvalue2 rvalue3 rvalue4 rvalue5 rvalue6 rvalue7)
    [(program stmt ...) (syntax/loc stx (program stmt ...))]
    [(statement "import" type name ";") (syntax/loc stx (import type name))]
    [(statement lv (assign ...) rv ";") (syntax/loc stx (assign ... lv rv))]
    [(statement cl ";") (syntax/loc stx cl)]
    [(statement "{" st ... "}") (syntax/loc stx (block st ...))]
    [(statement "~ATH" "(" obj ")" body "EXECUTE" "(" exec ")" ";") (syntax/loc stx (~ath obj body exec))]
    [(statement-or-rvalue x) (syntax/loc stx x)]
    [(rvalue-exec-wrapper x) (syntax/loc stx (exec x))]
    [(lvalue x) (syntax/loc stx (var-ref x))]
    [(lvalue x "." field) (syntax/loc stx (field-ref x field))]
    [(lvalue x "[" y "]") (syntax/loc stx (array-ref x y))]
    [(call func "(" arg ... ")") (quasisyntax/loc stx (call func . #,(filter-not (lambda (x) (equal? (syntax-e x) ","))
                                                                                 (syntax-e #'(arg ...)))))]
    [(assignment-op "=") (syntax/loc stx (var-set!))]
    [(assignment-op x "=") (syntax/loc stx (ext-set! x))]
    [(logic-op "&" "&") (syntax/loc stx land)]
    [(logic-op "|" "|") (syntax/loc stx lior)]
    [(logic-op "^" "^") (syntax/loc stx lxor)]
    [(comparison-op x y) (quasisyntax/loc stx (compare #,(string-append (syntax-e (syntax/loc stx x)) (syntax-e (syntax/loc stx y)))))]
    [(comparison-op x) (syntax/loc stx (compare x))]
    [(bin-op x) (syntax/loc stx (math x))]
    [(add-op x) (syntax/loc stx (math x))]
    [(mul-op x) (syntax/loc stx (math x))]
    [(unary-op x) (syntax/loc stx (umath x))]
    [(rvalue x) (syntax/loc stx x)]
    [(rvalue1 x) (syntax/loc stx x)]
    [(rvalue2 x) (syntax/loc stx x)]
    [(rvalue3 x) (syntax/loc stx x)]
    [(rvalue4 x) (syntax/loc stx x)]
    [(rvalue5 x) (syntax/loc stx x)]
    [(rvalue6 x) (syntax/loc stx x)]
    [(rvalue7 x) (syntax/loc stx x)]
    [(rvalue x l y) (syntax/loc stx (l x y))]
    [(rvalue1 x (c ...) y) (syntax/loc stx (c ... x y))]
    [(rvalue2 x (c ...) y) (syntax/loc stx (c ... x y))]
    [(rvalue3 x (c ...) y) (syntax/loc stx (c ... x y))]
    [(rvalue4 x (c ...) y) (syntax/loc stx (c ... x y))]
    [(rvalue7 "(" x ")") (syntax/loc stx x)]
    [(boolean "TRUE") (syntax/loc stx #t)]
    [(boolean "FALSE") (syntax/loc stx #f)]
    [(constant "NULL") (syntax/loc stx (constant-null))]
    [(constant x) (syntax/loc stx (constant x))]
    ))

(define (main ip)
  (simplify (parse (tokenize ip))))

;(define example (call-with-input-file "example.~ath" main))
;example
