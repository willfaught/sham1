(module haskell-compiler mzscheme
  (require (lib "haskell-terms.ss" "hs")
           (lib "list.ss" "srfi" "1")
           (lib "match.ss")
           (lib "test.ss" "hs"))
  
  (provide compile-haskell)
  
  (define compile-expression
    (match-lambda (($ application-term f a) (compile-eapp f (reverse a)))
                  (($ case-term e as) `(match ,(compile-expression e) ,@(map (lambda (a) `(,(string->symbol (car a)) ,(compile-expression (cdr a)))) as)))
                  (($ character-term c) (car (hash-table-get characters c (lambda () (list (string-ref c 0))))))
                  (($ declaration-term p e t) `(define ,(string->symbol (car p)) (delay ,(if (null? (cdr p)) (compile-expression e) (compile-expression (make-function-term (cdr p) e t))))))
                  (($ float-term f) (string->number f))
                  (($ function-term p b t) (if (null? p) (compile-expression b) `(match-lambda (,(string->symbol (car p)) ,(compile-expression (make-function-term (cdr p) b t))))))
                  (($ identifier-term i) (if (member i prelude) `(force ,(string->symbol (string-append "haskell:" i))) `(force ,(string->symbol i))))
                  (($ if-term g t e) `(if ,(compile-expression g) ,(compile-expression t) ,(compile-expression e)))
                  (($ integer-term i) (string->number i))
                  (($ let-term ds e) `(begin ,@(map compile-expression (filter (lambda (d) (not (equal? (car (declaration-term-patterns d)) "_"))) ds)) ,(compile-expression e)))
                  (($ list-term e) (if (null? e) null `(cons (delay ,(compile-expression (car e))) (delay ,(compile-expression (make-list-term (cdr e)))))))
                  (($ tuple-term e) (compile-expression (make-application-term (make-tuplecon-term (length e)) e)))
                  (($ tuplecon-term a) (compile-etupcon a))))
  
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
    (define decls (filter (lambda (x) (and (declaration-term? x) (not (equal? (car (declaration-term-patterns x)) "_")))) (module-term-declarations module)))
    `(module ,(string->symbol (module-term-identifier module)) mzscheme
       (require (lib "match.ss")
                (lib "haskell-prelude.ss" "hs"))
       (provide (all-defined))
       ,@(map compile-expression decls)))
  
  (define compilation-tests
    (list (make-test "eapp 1" (make-application-term (make-identifier-term "x") (list (make-integer-term "4"))) '((force x) (delay 4)))
          (make-test "eapp 2" (make-application-term (make-identifier-term "x") (list (make-integer-term "5") (make-integer-term "6"))) '(((force x) (delay 5)) (delay 6)))
          (make-test "ecase 1" (make-case-term (make-integer-term "2") (list (cons "x" (make-integer-term "3")))) '(match 2 (x 3)))
          (make-test "echar 1" (make-character-term "c") #\c)
          (make-test "edecl 1" (make-declaration-term (list "x") (make-integer-term "2") #f) '(define x (delay 2)))
          (make-test "edecl 2" (make-declaration-term (list "x" "y") (make-integer-term "2") #f) '(define x (delay (match-lambda (y 2)))))
          (make-test "edecl 3" (make-declaration-term (list "x" "_" "y") (make-integer-term "2") #f) '(define x (delay (match-lambda (_ (match-lambda (y 2)))))))
          (make-test "efun 1" (make-function-term (list "x") (make-identifier-term "x") #f) '(match-lambda (x (force x))))
          (make-test "efun 2" (make-function-term (list "x" "y") (make-identifier-term "x") #f) '(match-lambda (x (match-lambda (y (force x))))))
          (make-test "eid 1" (make-identifier-term "x") '(force x))
          (make-test "eid 2" (make-identifier-term "+") '(force haskell:+))
          (make-test "eif 1" (make-if-term (make-identifier-term "True") (make-integer-term "2") (make-integer-term "3")) '(if (force haskell:True) 2 3))
          (make-test "elet 1" (make-let-term (list (make-declaration-term (list "x") (make-integer-term "2") #f)) (make-integer-term "3")) '(begin (define x (delay 2)) 3))
          (make-test "elet 2" (make-let-term (list (make-declaration-term (list "x") (make-integer-term "2") #f) (make-declaration-term (list "y") (make-integer-term "3") #f)) (make-integer-term "4")) '(begin (define x (delay 2)) (define y (delay 3)) 4))
          (make-test "elist 1" (make-list-term null) '())
          (make-test "elist 2" (make-list-term (list (make-integer-term "2"))) '(cons (delay 2) (delay ())))
          (make-test "elist 3" (make-list-term (list (make-integer-term "2") (make-integer-term "3"))) '(cons (delay 2) (delay (cons (delay 3) (delay ())))))
          (make-test "enum 1" (make-integer-term "1") 1)
          (make-test "etup 1" (make-tuple-term (list (make-integer-term "2") (make-character-term "c"))) '(((lambda (x1) (lambda (x2) (vector-immutable x1 x2))) (delay 2)) (delay #\c)))
          (make-test "etupcon 1" (make-tuplecon-term 2) '(lambda (x1) (lambda (x2) (vector-immutable x1 x2))))
          (make-test "etupcon 2" (make-tuplecon-term 3) '(lambda (x1) (lambda (x2) (lambda (x3) (vector-immutable x1 x2 x3)))))))
  
  (define (run-all-tests)
    (run-tests (lambda (x) (compile-expression x)) compilation-tests)))