(module haskell-compiler mzscheme
  (require (lib "match.ss"))
  (require (lib "haskell-prelude.ss" "hs"))
  
  ;(provide (all-defined))

  (define-struct tapp (function arguments))
  ;(define-struct tcase (expression alternates))
  (define-struct tchar (value))
  (define-struct tfun (parameters body))
  (define-struct tfdef (name function))
  (define-struct tid (name))
  ;(define-struct tlet (bindings body))
  (define-struct tlist (expressions))
  (define-struct tmod (name declarations))
  (define-struct tnum (value))
  (define-struct ttup (expressions))
  
  (define compile-haskell
    (match-lambda (($ tapp f a) (compile-tapp f (reverse a)))
                  (($ tchar v) (compile-tchar v))
                  (($ tfun p b) (if (null? p) (compile-haskell b) `(lambda (,(string->symbol (car p))) ,(compile-haskell (make-tfun (cdr p) b)))))
                  (($ tfdef n f) `(define ,(string->symbol n) ,(compile-haskell f)))
                  (($ tid n) (car (hash-table-get prelude n (lambda () (list `(force ,(string->symbol n)))))))
                  (($ tlist es) `',(map (lambda (e) (delay (compile-haskell e))) es))
                  (($ tnum v) v)
                  (($ ttup es) `',(map (lambda (e) (compile-haskell e)) es))))
  
  (define ch compile-haskell)
  
  (define (compile-tapp f a)
    (if (null? a) (compile-haskell f) `(,(compile-tapp f (cdr a)) (delay ,(compile-haskell (car a))))))
  
  (define (compile-tchar v)
    (string-ref v 0))
  
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