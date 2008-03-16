(module haskell-compiler mzscheme
  (require (lib "match.ss"))
  (require (lib "haskell-prelude.ss" "hs"))
  
  (provide (all-defined))

  (define-struct tapp (function arguments))
  (define-struct tcase (expression alternates))
  (define-struct tchar (character))
  (define-struct tfun (parameters body))
  (define-struct tfdef (identifier expression))
  (define-struct tid (identifier))
  (define-struct tlet (bindings expression))
  (define-struct tlist (expressions))
  (define-struct tmod (name declarations))
  (define-struct tnum (number))
  (define-struct tpat (pattern))
  (define-struct ttup (expressions))
  
  ; notes:
  ; make tuple creation a type of fun app.  support multi commas.
  
  (define compile-haskell
    (match-lambda (($ tapp f a) (compile-tapp f (reverse a)))
                  (($ tcase e as) `(match ,(compile-haskell e) ,@(map (lambda (a) `(,(car a) ,(compile-haskell (cdr a)))) as)))
                  (($ tchar c) (car (hash-table-get characters c (lambda () (list (string-ref c 0))))))
                  (($ tfun p b) (if (null? p) (compile-haskell b) `(lambda (,(string->symbol (car p))) ,(compile-haskell (make-tfun (cdr p) b)))))
                  (($ tfdef i e) `(define ,(string->symbol i) ,(compile-haskell e)))
                  (($ tid i) (car (hash-table-get prelude i (lambda () (list `(force ,(string->symbol i)))))))
                  (($ tlet bs e) `(letrec ,(map (lambda (b) `(,(string->symbol (car b)) (delay ,(compile-haskell (cdr b))))) bs) ,(compile-haskell e)))
                  (($ tlist es) `(list ,@(map (lambda (e) `(delay ,(compile-haskell e))) es)))
                  (($ tnum n) n)
                  (($ ttup es) `(vector ,@(map (lambda (e) `(delay ,(compile-haskell e))) es)))))
  
  (define ch compile-haskell)
  
  (define (compile-tapp f a)
    (if (null? a) (compile-haskell f) `(,(compile-tapp f (cdr a)) (delay ,(compile-haskell (car a))))))
  
  (define characters
    (make-immutable-hash-table `() 'equal))
    
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