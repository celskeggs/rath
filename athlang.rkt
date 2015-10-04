#lang racket
(require "athlibs.rkt")
(require racket/stxparam)
(require racket/fixnum)

(provide (rename-out [mod-begin #%module-begin])
         program import call block
         var-set! ext-set!
         var-ref field-ref array-ref
         constant constant-null
         ~ath exec
         land lior lxor
         compare math)

(define-syntax-parameter local-vars (syntax-id-rules () [_ null]))

(define-syntax-rule (mod-begin x)
  (#%module-begin (block (var-set! (var-ref THIS) (new-this)) x (error "Program did not terminate properly!"))))

(define-syntax-rule (program xes ...)
  (block xes ...))

(define-syntax-rule (block xes ...)
  (let ((local-env (cons (make-hash) local-vars)))
    (syntax-parameterize ([local-vars (syntax-id-rules () [_ local-env])])
                         xes ...)))

(define-syntax-rule (import type name)
  (var-set! (var-ref name) (do-import 'type 'name)))

(define-syntax-rule (call func arg ...)
  (func arg ...))

(define-syntax var-set!
  (syntax-rules (field-ref array-ref)
    [(var-set! (field-ref obj name) value)
     (prop-set! obj 'name value)]
    [(var-set! (array-ref obj index) value)
     (prop-set-index! obj index value)]
    [(var-set! (var-ref name) value) ; TODO: ensure that 'name is a symbol
     (if (symbol? 'name)
         (var-store! local-vars 'name value)
         (error 'OOPS_NOT_A_SYMBOL 'name))]))

(define-syntax ext-set!
  (syntax-rules (field-ref array-ref var-ref)
    [(ext-set! (proc ...) (field-ref obj name) value)
     (let ((cobj obj))
       (prop-set! cobj 'name (proc ... (field-ref cobj name) value)))]
    [(ext-set! (proc ...) (array-ref obj index) value)
     (let ((cobj obj) (cindex index))
       (prop-set-index! cobj cindex (proc ... (array-ref cobj cindex) value)))]
    [(ext-set! (proc ...) (var-ref name) value)
     (var-store! local-vars 'name (proc ... (var-ref name) value))]))

(define (var-lookup env name)
  (if (null? env)
      (error "no such variable: " name)
      (hash-ref (car env) name (lambda () (var-lookup (cdr env) name)))))

(define (var-store! env name value)
  (if (null? env)
      (error "no such variable: " name)
      (if (and (var-exists? (cdr env) name) (not (hash-has-key? (car env) name)))
          (var-store! (cdr env) name value)
          (hash-set! (car env) name value))))

(define (var-exists? env name)
  (and (not (null? env))
       (or (hash-has-key? (car env) name)
           (var-exists? (cdr env) name))))

(define-syntax-rule (var-ref name)
  (var-lookup local-vars 'name))

(define-syntax-rule (field-ref value name)
  (prop-ref value 'name))

(define-syntax-rule (array-ref value index)
  (prop-index value index))

(define-syntax-rule (constant x)
  (#%datum . x))

(define-syntax-rule (constant-null)
  (void))

(define-syntax-rule (~ath object body exec)
  (when (is-alive? object)
    (let iter ()
      body
      (when (is-alive? object)
        (iter)))
    exec))

(define (execute x)
  (cond ((void? x) (void))
        ((procedure? x) (x)) ; TODO: make this worth something
        (else (error "Cannot execute" x))))

(define-syntax-rule (exec x)
  (execute x))

(define-syntax-rule (land a b)
  (and a b))

(define-syntax-rule (lior a b)
  (or a b))

(define (xor a b)
  (if a (not b)
      (not (not b))))

(define-syntax-rule (lxor a b)
  (xor a b))

(define-syntax compare
  (syntax-rules ()
    [(compare "==" a b) (eqv? a b)]
    [(compare "!=" a b) (not (eqv? a b))]
    [(compare ">=" a b) (>= a b)]
    [(compare "<=" a b) (<= a b)]
    [(compare "<" a b) (< a b)]
    [(compare ">" a b) (> a b)]))

(define-syntax math
  (syntax-rules ()
    [(math "&" a b) (fxand a b)]
    [(math "|" a b) (fxior a b)]
    [(math "^" a b) (fxxor a b)]
    [(math "+" a b) (fx+ a b)]
    [(math "-" a b) (fx- a b)]
    [(math "*" a b) (fx* a b)]
    [(math "/" a b) (fxquotient a b)]))
