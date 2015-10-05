#lang racket
(require racket/fixnum)

(provide new-this do-import is-alive? prop-ref prop-set! prop-has-key? prop-index prop-set-index!)

(define (new-this)
  (hash 'DIE (lambda () (error "Program terminated."))))

(define (prop-ref x name [fail (lambda () (error "no such field" name 'on x))])
  (cond ((hash? x) (hash-ref x name fail))
        ((string? x)
         (case name
           ((length) (string-length x))
           (else (if (procedure? fail) (fail) fail))))
        (else (if (procedure? fail) (fail) fail))))
(define (prop-has-key? x name)
  (if (hash? x) (hash-has-key? x name)
      (let ((has #t))
        (prop-ref x name (lambda () (set! has #f)))
        has)))
(define (prop-set! x name value)
  (if (hash? x)
      (hash-set! x name value)
      (if (prop-has-key? x name)
          (error "field is not mutable:" name)
          (error "no such field" name))))
(define (prop-index x index)
  (cond ((vector? x) (vector-ref x index))
        ((string? x) (char->integer (string-ref x index)))
        (else (error "object is not indexable" x))))
(define (prop-set-index! x index value)
  (cond ((vector? x) (vector-set! x index value))
        ((string? x) (error "strings are immutable"))
        (else (error "object is not indexable" x))))

(define-syntax-rule (invoke x name params ...)
  ((prop-ref x 'name) params ...))

(define (is-alive? x)
  (if (prop-has-key? x 'is-alive?)
      (invoke x is-alive?)
      #t))

(define modules (make-hash))
(define (add-module! typename value)
  (hash-set! modules typename value))
(define-syntax-rule (def-module (typename name) body ...)
  (add-module! 'typename (lambda (name) body ...)))
(define-syntax-rule (def-module-each typename (name expr) ...)
  (def-module (typename iname)
    (case iname
      [(name) expr] ...
      (else (error "procedure not found in module" 'typename)))))

(define (do-import typename name)
  ((hash-ref modules typename
             (lambda () (error "no such module:" typename)))
   name))

(define (my-print . xes)
  (map display xes)
  (displayln ""))

(def-module-each procedure
  [print my-print])

(define (wrap-input-port port)
  (hash 'readline (lambda () (read-line port))
        'read (lambda () (read-byte))))

(define (wrap-output-port port)
  (hash 'write (lambda (byte) (write-byte byte port))))

(def-module-each stream
  [stdin (wrap-input-port (current-input-port))]
  [stdout (wrap-output-port (current-output-port))]
  [stderr (wrap-output-port (current-error-port))])

(def-module (turingtape name)
  (define contents empty)
  (hash 'acquire (lambda () (begin0 contents (set! contents (void))))
        'release (lambda (x) (set! contents x))))

(def-module (turingtapehead name)
  (define left (void))
  (define right (void))
  (hash 'insert (lambda (x)
                  (set! right (invoke x acquire))
                  (set! left empty))
        'remove (lambda (x)
                  (invoke x release (append (reverse left) right))
                  (set! left (void))
                  (set! right (void)))
        'read (lambda ()
                (if (empty? right) 0
                    (car right)))
        'erase (lambda ()
                 (unless (or (empty? right) (= (car right) 0))
                   (set! right (cons 0 (cdr right)))))
        'write (lambda (x)
                 (unless (fixnum? x)
                   (error "only integers can be written to the tape!"))
                 (if (empty? right)
                     (set! right (list x))
                     (set! right (cons (fxior (car right) x) (cdr right)))))
        'play (lambda ([n 1])
                (for ((k n))
                  (if (empty? right)
                      (set! left (cons 0 left))
                      (begin
                        (set! left (cons (car right) left))
                        (set! right (cdr right))))))
        'rewind (lambda ([n 1])
                (for ((k n))
                  (if (empty? left)
                      (set! right (cons 0 right))
                      (begin
                        (set! right (cons (car left) right))
                        (set! left (cdr left))))))))

(def-module (bit name)
  (define value #f)
  (hash 'bind (lambda (x) (set! value (not (not x))))
        'is-alive? (lambda () value)))
