(module scheme-emitter mzscheme
  (require (lib "match.ss"))
  (provide (all-defined))

  (define-struct term (syntax))
  (define-struct (tapp term) (function argument))
  (define-struct (tcase term) (expression patterns))
  (define-struct (tchar term) (value))
  (define-struct (tfun term) (parameter body))
  (define-struct (tid term) (name))
  (define-struct (tlet term) (bindings body))
  (define-struct (tlist term) (head tail))
  (define-struct (tboundary term) (expression))
  (define-struct (tnum term) (value))
  (define-struct (ttup term) (elements))
  (define-struct (ttype term) (name constructors))
  
  (define emit-scheme (match-lambda (($ tnum s v) `,v)
                                    (($ tchar s v) `,v)
                                    (($ tid s n) (emit-hid n))
                                    (($ tfun s p b) `(lambda (,(string->symbol p)) ,(emit-scheme b)))
                                    (($ tapp s f a) `(,(emit-scheme f) (delay ,(emit-scheme a))))))
  
  (define (emit-hid id) (cond ((equal? id "+") '+)
                              ((equal? id "-") '-)
                              ((equal? id "*") '*)
                              ((equal? id "/") '/)
                              (else `(force ,(string->symbol id)))))
  
  ;(define (emit-scheme-list c)
  ;  (define (unnest c) (if (eqv? c 'nil) () `(delay ,(emit-scheme (hcons-head c)))
  
  (define (run-test hexp result) (eqv? (eval (emit-scheme hexp)) result))
  
  (define (run-tests tests) (map (lambda (test) (run-test (car test) (cdr test))) tests))
  
  ;(define tests (list (cons (make-hnum 1) 1)
  ;                    (cons (make-happ (make-hfun "x" (make-hid "x")) (make-hnum 1)) 1)))
)