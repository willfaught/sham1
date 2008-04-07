(module haskell-compiler mzscheme
  (require (lib "list.ss")
           (lib "match.ss"))
  
  (provide (all-defined))
  
  (define-struct eapp (function arguments))
  (define-struct ecase (expression alternates))
  (define-struct echar (character))
  (define-struct edecl (patterns expression))
  (define-struct efun (patterns expression))
  (define-struct eid (identifier))
  (define-struct eif (guard then else))
  (define-struct elet (declarations expression))
  (define-struct elist (expressions))
  (define-struct emod (identifier declarations))
  (define-struct enum (number))
  (define-struct etup (expressions))
  (define-struct etupcon (arity))
  
  (define-struct tabs (parameters expression))
  (define-struct tapp (abstraction arguments))
  (define-struct tchar (expression))
  (define-struct tfloat (expression))
  (define-struct tint (expression))
  (define-struct tlist (type expressions))
  (define-struct ttuple (types expressions))
  (define-struct tvar (identifier))
  
  (define (check-types expression) 0)
  
  (define (erase-types expression) 0)
  
  (define compile-expression
    (match-lambda (($ eapp f a) (compile-eapp f (reverse a)))
                  (($ ecase e as) `(match ,(compile-expression e) ,@(map (lambda (a) `(,(string->symbol (car a)) ,(compile-expression (cdr a)))) as)))
                  (($ echar c) (car (hash-table-get characters c (lambda () (list (string-ref c 0))))))
                  (($ edecl p e) `(define ,(string->symbol (car p)) (delay ,(if (null? (cdr p)) (compile-expression e) (compile-expression (make-efun (cdr p) e))))))
                  (($ efun p e) (if (null? p) (compile-expression e) `(match-lambda (,(string->symbol (car p)) ,(compile-expression (make-efun (cdr p) e))))))
                  (($ eid i) (if (member i prelude) `(force ,(string->symbol (string-append "haskell:" i))) `(force ,(string->symbol i))))
                  (($ eif g t e) `(if ,(compile-expression g) ,(compile-expression t) ,(compile-expression e)))
                  (($ elet ds e) `(begin ,@(map compile-expression (filter (lambda (d) (not (equal? (car (edecl-patterns d)) "_"))) ds)) ,(compile-expression e)))
                  (($ elist e) (if (null? e) null `(cons (delay ,(compile-expression (car e))) (delay ,(compile-expression (make-elist (cdr e)))))))
                  (($ enum n) (string->number n))
                  (($ etup e) (compile-expression (make-eapp (make-etupcon (length e)) e)))
                  (($ etupcon a) (compile-etupcon a))))
  
  (define (compile-eapp f a)
    (if (null? a) (compile-expression f) `(,(compile-eapp f (cdr a)) (delay ,(compile-expression (car a))))))
  
  (define (compile-etupcon a)
    (define (enumerate n)
      (if (= n 0) null (cons n (enumerate (- n 1)))))
    (define (nest n)
      (if (= n (+ a 1))
          `(vector-immutable ,@(map (lambda (x) (string->symbol (string-append "x" (number->string x)))) (reverse (enumerate a))))
          `(lambda (,(string->symbol (string-append "x" (number->string n)))) ,(nest (+ n 1)))))
    (nest 1))
  
  (define ce compile-expression)
  
  (define characters
    (make-immutable-hash-table `() 'equal))
  
  (define prelude '("error" "print" "+" "-" "*" "/" "==" "/=" ":" "head" "tail" "null" "fst" "snd" "True" "False" "&&" "||" "not"))
  
  (define (compile-haskell module)
    (define decls (filter (lambda (x) (and (edecl? x) (not (equal? (car (edecl-patterns x)) "_")))) (emod-declarations module)))
    `(module ,(string->symbol (emod-identifier module)) mzscheme
       (require (lib "match.ss")
                (lib "haskell-prelude.ss" "hs"))
       (provide (all-defined))
       ,@(map compile-expression decls)))
  
  (define-struct test (name expression result))
  
  (define (run-test f)
    (lambda (test)
      (define result (f (test-expression test)))
      (if (equal? result (test-result test)) #t (format "\n~a failed\n  expected: ~a\n  actual: ~a\n" (test-name test) (test-result test) result))))
  
  (define (run-tests f tests)
    (define results (filter (lambda (x) (not (equal? x #t))) (map (run-test f) tests)))
    (if (null? results)
        (display "All passed")
        (map display results)))
  
  (define compilation-tests
    (list (make-test "eapp 1" (make-eapp (make-eid "x") (list (make-enum "4"))) '((force x) (delay 4)))
          (make-test "eapp 2" (make-eapp (make-eid "x") (list (make-enum "5") (make-enum "6"))) '(((force x) (delay 5)) (delay 6)))
          (make-test "ecase 1" (make-ecase (make-enum "2") (list (cons "x" (make-enum "3")))) '(match 2 (x 3)))
          (make-test "echar 1" (make-echar "c") #\c)
          (make-test "edecl 1" (make-edecl (list "x") (make-enum "2")) '(define x (delay 2)))
          (make-test "edecl 2" (make-edecl (list "x" "y") (make-enum "2")) '(define x (delay (match-lambda (y 2)))))
          (make-test "edecl 3" (make-edecl (list "x" "_" "y") (make-enum "2")) '(define x (delay (match-lambda (_ (match-lambda (y 2)))))))
          (make-test "efun 1" (make-efun (list "x") (make-eid "x")) '(match-lambda (x (force x))))
          (make-test "efun 2" (make-efun (list "x" "y") (make-eid "x")) '(match-lambda (x (match-lambda (y (force x))))))
          (make-test "eid 1" (make-eid "x") '(force x))
          (make-test "eid 2" (make-eid "+") '(force haskell:+))
          (make-test "eif 1" (make-eif (make-eid "True") (make-enum "2") (make-enum "3")) '(if (force haskell:True) 2 3))
          (make-test "elet 1" (make-elet (list (make-edecl (list "x") (make-enum "2"))) (make-enum "3")) '(begin (define x (delay 2)) 3))
          (make-test "elet 2" (make-elet (list (make-edecl (list "x") (make-enum "2")) (make-edecl (list "y") (make-enum "3"))) (make-enum "4")) '(begin (define x (delay 2)) (define y (delay 3)) 4))
          (make-test "elist 1" (make-elist null) '())
          (make-test "elist 2" (make-elist (list (make-enum "2"))) '(cons (delay 2) (delay ())))
          (make-test "elist 3" (make-elist (list (make-enum "2") (make-enum "3"))) '(cons (delay 2) (delay (cons (delay 3) (delay ())))))
          (make-test "enum 1" (make-enum "1") 1)
          (make-test "etup 1" (make-etup (list (make-enum "2") (make-echar "c"))) '(((lambda (x1) (lambda (x2) (vector x1 x2))) (delay 2)) (delay #\c)))
          (make-test "etupcon 1" (make-etupcon 2) '(lambda (x1) (lambda (x2) (vector x1 x2))))
          (make-test "etupcon 2" (make-etupcon 3) '(lambda (x1) (lambda (x2) (lambda (x3) (vector x1 x2 x3)))))))
  
  (define (run-all-tests)
    (run-tests (lambda (x) (compile-expression x)) compilation-tests)))