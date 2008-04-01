(module haskell-compiler mzscheme
  (require (lib "list.ss")
           (lib "match.ss"))
  
  (provide (all-defined))
  
  (define-struct tapp (function arguments))
  (define-struct tcase (expression alternates))
  (define-struct tchar (character))
  (define-struct tdecl (patterns expression))
  (define-struct tfun (patterns expression))
  (define-struct tid (identifier))
  (define-struct tlet (declarations expression))
  (define-struct tlist (expressions))
  (define-struct tmod (identifier declarations))
  (define-struct tnum (number))
  (define-struct ttup (expressions))
  (define-struct ttupcon (arity))
  
  ; check if list construction syntactic sugar works for fib when conditionals done.
  
  (define compile-expression
    (match-lambda (($ tapp f a) (compile-tapp f (reverse a)))
                  (($ tcase e as) `(match ,(compile-expression e) ,@(map (lambda (a) `(,(string->symbol (car a)) (delay ,(compile-expression (cdr a))))) as)))
                  (($ tchar c) (car (hash-table-get characters c (lambda () (list (string-ref c 0))))))
                  (($ tdecl p e) `(define ,(string->symbol (car p)) (delay ,(if (null? (cdr p)) (compile-expression e) (compile-expression (make-tfun (cdr p) e))))))
                  (($ tfun p e) (if (null? p) (compile-expression e) `(match-lambda (,(string->symbol (car p)) ,(compile-expression (make-tfun (cdr p) e))))))
                  (($ tid i) (if (member i prelude) `(force ,(string->symbol (string-append "haskell:" i))) `(force ,(string->symbol i))))
                  (($ tlet ds e) `(begin ,(map compile-expression (filter (lambda (d) (not (equal? (car (tdecl-patterns d)) "_"))) ds)) ,(compile-expression e)))
                  (($ tlist es) `(list ,@(map (lambda (e) `(delay ,(compile-expression e))) es)))
                  (($ tnum n) (string->number n))
                  (($ ttup e) (compile-expression (make-tapp (make-ttupcon (length e)) e)))
                  (($ ttupcon a) (compile-ttupcon a))))
  
  (define (compile-tapp f a)
    (if (null? a) (compile-expression f) `(,(compile-tapp f (cdr a)) (delay ,(compile-expression (car a))))))
  
  (define (compile-ttupcon a)
    (define (enumerate n)
      (if (= n 0) null (cons n (enumerate (- n 1)))))
    (define (nest n)
      (if (= n (+ a 1))
          `(vector ,@(map (lambda (x) (string->symbol (string-append "x" (number->string x)))) (reverse (enumerate a))))
          `(lambda (,(string->symbol (string-append "x" (number->string n)))) ,(nest (+ n 1)))))
    (nest 1))
  
  (define ce compile-expression)
  
  (define characters
    (make-immutable-hash-table `() 'equal))
  
  (define prelude '("+" "-" "*" "/" ":" "head" "tail" "fst" "snd"))
  
  (define (compile-haskell module)
    (define decls (filter (lambda (x) (and (tdecl? x) (not (equal? (car (tdecl-patterns x)) "_")))) (tmod-declarations module)))
    `(module ,(string->symbol (tmod-identifier module)) mzscheme
       (require (lib "match.ss")
                (lib "haskell-prelude.ss" "hs"))
       (provide (all-defined))
       ,@(map compile-expression decls)))
  
  (define (run-test exp result)
    (equal? (eval (compile-expression exp)) result))
  
  (define (run-tests tests)
    (map (lambda (test) (run-test (car test) (cdr test))) tests))
  
  ;(define tests (list (cons (make-hnum 1) 1)
  ;                    (cons (make-happ (make-hfun "x" (make-hid "x")) (make-hnum 1)) 1)))
  )