(module haskell-compiler mzscheme
  (require (lib "match.ss"))
  (provide (all-defined))

  (define-struct tapp (function arguments))
  (define-struct tcase (expression alternates))
  (define-struct tchar (value))
  (define-struct tfun (parameters body))
  (define-struct tfdecl (name parameters body))
  (define-struct tid (name))
  (define-struct tlet (bindings body))
  (define-struct tlist (expressions))
  (define-struct tmod (name declarations))
  (define-struct tnum (value))
  (define-struct ttup (expressions))
  
  (define compile-haskell
    (match-lambda (($ tapp f a) `(,(compile-haskell f) (delay ,(compile-haskell a))))
                  (($ tchar v) (compile-tchar v))
                  (($ tfun p b) `(lambda (,(string->symbol p)) ,(compile-haskell b)))
                  (($ tid n) (car (hash-table-get prelude n (lambda () (list `(force ,(string->symbol n)))))))
                  (($ tlist es) (map (lambda (e) (compile-haskell e)) es))
                  (($ tmod n d) (compile-tmod n d))
                  (($ tnum v) v)
                  (($ ttup es) (map (lambda (e) (compile-haskell e)) es))))
  
  (define (compile-tchar c)
    (string-ref c 0))
  
  (define (compile-tmod n d)
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