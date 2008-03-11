(module haskell-compiler mzscheme
  (require (lib "match.ss"))
  (provide (all-defined))

  (define-struct tapp (function argument))
  (define-struct tchar (value))
  (define-struct tfun (parameter body))
  (define-struct tid (name))
  (define-struct tlist (head tail))
  (define-struct tmod (name declarations))
  (define-struct tnum (value))
  
  ;BUG: nomatch for 'empty-list
  (define compile-haskell
    (match-lambda (($ tapp f a) `(,(compile-haskell f) (delay ,(compile-haskell a))))
                  (($ tchar v) (string-ref v 0))
                  (($ tfun p b) `(lambda (,(string->symbol p)) ,(compile-haskell b)))
                  (($ tid n) (car (hash-table-get prelude n (lambda () (list `(force ,(string->symbol n)))))))
                  (($ tlist h t) `',(compile-tlist h t))
                  (($ tmod n d) (compile-tmod n d))
                  (($ tnum v) v)))
  
  (define (compile-tlist head tail)
    (cond ((equal? head 'empty-list) '())
          ((equal? tail 'empty-list) (compile-haskell head))
          (else (cons (compile-haskell head) (compile-tlist (tlist-head tail) (tlist-tail tail))))))
  
  (define (compile-tmod name declarations)
    1)
  
  (define prelude
    (make-immutable-hash-table `(("+" (lambda (x) (lambda (y) (+ (force x) (force y)))))
                                 ("-" (lambda (x) (lambda (y) (- (force x) (force y)))))
                                 ("*" (lambda (x) (lambda (y) (* (force x) (force y)))))
                                 ("/" (lambda (x) (lambda (y) (/ (force x) (force y)))))
                                 (":" (lambda (h) (lambda (t) (cons h t))))) 'equal))
  
  (define (run-test exp result)
    (equal? (eval (compile-haskell exp)) result))
  
  (define (run-tests tests)
    (map (lambda (test) (run-test (car test) (cdr test))) tests))
  
  ;(define tests (list (cons (make-hnum 1) 1)
  ;                    (cons (make-happ (make-hfun "x" (make-hid "x")) (make-hnum 1)) 1)))
)