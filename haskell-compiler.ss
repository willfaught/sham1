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
  (define-struct tif (guard then else))
  (define-struct tlet (declarations expression))
  (define-struct tlist (expressions))
  (define-struct tmod (identifier declarations))
  (define-struct tnum (number))
  (define-struct ttup (expressions))
  (define-struct ttupcon (arity))
  
  (define compile-expression
    (match-lambda (($ tapp f a) (compile-tapp f (reverse a)))
                  (($ tcase e as) `(match ,(compile-expression e) ,@(map (lambda (a) `(,(string->symbol (car a)) ,(compile-expression (cdr a)))) as)))
                  (($ tchar c) (car (hash-table-get characters c (lambda () (list (string-ref c 0))))))
                  (($ tdecl p e) `(define ,(string->symbol (car p)) (delay ,(if (null? (cdr p)) (compile-expression e) (compile-expression (make-tfun (cdr p) e))))))
                  (($ tfun p e) (if (null? p) (compile-expression e) `(match-lambda (,(string->symbol (car p)) ,(compile-expression (make-tfun (cdr p) e))))))
                  (($ tid i) (if (member i prelude) `(force ,(string->symbol (string-append "haskell:" i))) `(force ,(string->symbol i))))
                  (($ tif g t e) `(if ,(compile-expression g) ,(compile-expression t) ,(compile-expression e)))
                  (($ tlet ds e) `(begin ,@(map compile-expression (filter (lambda (d) (not (equal? (car (tdecl-patterns d)) "_"))) ds)) ,(compile-expression e)))
                  (($ tlist e) (if (null? e) null `(cons (delay ,(compile-expression (car e))) (delay ,(compile-expression (make-tlist (cdr e)))))))
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
  
  (define prelude '("error" "print" "+" "-" "*" "/" "==" "/=" ":" "head" "tail" "null" "fst" "snd" "True" "False" "&&" "||" "not"))
  
  (define (compile-haskell module)
    (define decls (filter (lambda (x) (and (tdecl? x) (not (equal? (car (tdecl-patterns x)) "_")))) (tmod-declarations module)))
    `(module ,(string->symbol (tmod-identifier module)) mzscheme
       (require (lib "match.ss")
                (lib "haskell-prelude.ss" "hs"))
       (provide (all-defined))
       ,@(map compile-expression decls)))
  
  (define (run-test exp result)
    (equal? (compile-expression exp) result))
  
  (define (run-tests tests)
    (map (lambda (test) (run-test (car test) (cdr test))) tests))
  
  (define tests (list (cons (make-tapp (make-tid "x") (list (make-tnum "4"))) '((force x) (delay 4)))
                      (cons (make-tapp (make-tid "x") (list (make-tnum "5") (make-tnum "6"))) '(((force x) (delay 5)) (delay 6)))
                      (cons (make-tcase (make-tnum "2") (list (cons "x" (make-tnum "3")))) '())
                      (cons (make-tnum "1") 1)
                      (cons (make-tchar "c") #\c)
                      (cons (make-tid "x") '(force x))
                      (cons (make-tid "+") '(force haskell:+))
                      (cons (make-tlist null) '())
                      (cons (make-tlist (list (make-tnum "1") (make-tnum "2") (make-tnum "3"))) '(cons (delay 1) (delay (cons (delay 2) (delay (cons (delay 3) (delay ())))))))
                      (cons (make-ttupcon 2) '(lambda (x1) (lambda (x2) (vector x1 x2))))
                      (cons (make-ttupcon 3) '(lambda (x1) (lambda (x2) (lambda (x3) (vector x1 x2 x3)))))
                      (cons (make-ttup (list (make-tnum "2") (make-tchar "c"))) '(((lambda (x1) (lambda (x2) (vector x1 x2))) (delay 2)) (delay #\c)))
                      (cons (make-tfun (list "x") (make-tid "x")) '(match-lambda (x (force x))))
                      (cons (make-tfun (list "x" "y") (make-tid "x")) '(match-lambda (x (match-lambda (y (force x))))))
                      ))
  )