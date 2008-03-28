(module haskell-compiler mzscheme
  (require (lib "list.ss")
           (lib "match.ss")
           (lib "haskell-prelude.ss" "hs"))
  
  (provide (all-defined))

  (define-struct tapp (function arguments))
  (define-struct tcase (expression alternates))
  (define-struct tchar (character))
  (define-struct tfun (patterns body))
  (define-struct tfdef (identifier expression))
  (define-struct tid (identifier))
  (define-struct tlet (bindings expression))
  (define-struct tlist (expressions))
  (define-struct tmod (identifier definitions))
  (define-struct tnum (number))
  (define-struct ttup (expressions))
  (define-struct ttupcon (arity))
  
  ; check if list construction syntactic sugar works for fib when conditionals done.
  
  (define compile-expression
    (match-lambda (($ tapp f a) (compile-tapp f (reverse a)))
                  (($ tcase e as) `(match ,(compile-expression e) ,@(map (lambda (a) `(,(string->symbol (car a)) (delay ,(compile-expression (cdr a))))) as)))
                  (($ tchar c) (car (hash-table-get characters c (lambda () (list (string-ref c 0))))))
                  (($ tfun p b) (if (null? p) (compile-expression b) `(match-lambda (,(string->symbol (car p)) ,(compile-expression (make-tfun (cdr p) b))))))
                  (($ tfdef i e) `(define ,(string->symbol i) ,(compile-expression e)))
                  (($ tid i) (car (hash-table-get prelude i (lambda () (list `(force ,(string->symbol i)))))))
                  (($ tlet bs e) `(match-letrec ,(map (lambda (b) `(,(string->symbol (car b)) (delay ,(compile-expression (cdr b))))) bs) ,(compile-expression e)))
                  (($ tlist es) `(list ,@(map (lambda (e) `(delay ,(compile-expression e))) es)))
                  (($ tnum n) n)
                  (($ ttup e) (compile-expression (make-tapp (make-ttupcon (length e)) e)))
                  (($ ttupcon a) (compile-ttupcon a))))
  
  (define (compile-ttupcon a)
    (define (enumerate n)
      (if (= n 0) null (cons n (enumerate (- n 1)))))
    (define (nest n)
      (if (= n (+ a 1))
          `(vector ,@(map (lambda (x) (string->symbol (string-append "x" (number->string x)))) (reverse (enumerate a))))
          `(lambda (,(string->symbol (string-append "x" (number->string n)))) ,(nest (+ n 1)))))
    (nest 1))
  
  (define ce compile-expression)
  
  (define (compile-tapp f a)
    (if (null? a) (compile-expression f) `(,(compile-tapp f (cdr a)) (delay ,(compile-expression (car a))))))
  
  (define characters
    (make-immutable-hash-table `() 'equal))
    
  (define prelude
    (make-immutable-hash-table `(("+" (lambda (x) (lambda (y) (+ (force x) (force y)))))
                                 ("-" (lambda (x) (lambda (y) (- (force x) (force y)))))
                                 ("*" (lambda (x) (lambda (y) (* (force x) (force y)))))
                                 ("/" (lambda (x) (lambda (y) (/ (force x) (force y)))))
                                 (":" (lambda (h) (lambda (t) (cons h t))))
                                 ("head" (lambda (l) (car (force l))))
                                 ("tail" (lambda (l) (cdr (force l))))
                                 ("fst" (lambda (t) (vector-ref (force t) 0)))
                                 ("snd" (lambda (t) (vector-ref (force t) 1)))) 'equal))
  
  (define (compile-haskell module)
    (define fdefs (filter tfdef? (tmod-definitions module)))
    #`(module #,(string->symbol (tmod-identifier module)) mzscheme
        (provide (all-defined))
        #,@(map compile-expression fdefs)))
  
  (define (run-test exp result)
    (equal? (eval (compile-expression exp)) result))
  
  (define (run-tests tests)
    (map (lambda (test) (run-test (car test) (cdr test))) tests))
  
  ;(define tests (list (cons (make-hnum 1) 1)
  ;                    (cons (make-happ (make-hfun "x" (make-hid "x")) (make-hnum 1)) 1)))
)