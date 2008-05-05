(module compiler-test mzscheme
  (require (only (lib "1.ss" "srfi") zip)
           (lib "compiler.ss" "haskell")
           (lib "list.ss")
           (lib "match.ss")
           (lib "reader.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "types.ss" "haskell")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide run-tests)
  
  ; test-case-e :: string string 'a -> schemeunit-test-case
  (define (test-case-e name expression value)
    (test-check name strict-equal? (eval (compile-term (parse-expression expression))) value))
  
  ; test-case-he :: string string string 'a -> schemeunit-test-case
  (define (test-case-he name type expression value)
    (test-equal? name (eval (compile-term (make-haskell-term (parse-type type) (parse-expression expression)))) value))
  
  ; test-case-hp :: string string string ('a -> boolean) -> schemeunit-test-case
  (define (test-case-hp name type expression predicate)
    (test-pred name predicate (eval (compile-term (make-haskell-term (parse-type type) (parse-expression expression))))))
  
  ; test-case-hx :: string string string -> schemeunit-test-case
  (define (test-case-hx name type expression)
    (test-exn name (lambda (x) #t) (lambda () (eval (compile-term (make-haskell-term (parse-type type) (parse-expression expression)))))))
  
  ; test-case-p :: string string ('a -> boolean) -> schemeunit-test-case
  (define (test-case-p name expression predicate)
    (test-pred name predicate (eval (compile-term (parse-expression expression)))))
  
  ; test-case-x :: string string -> schemeunit-test-case
  (define (test-case-x name expression)
    (test-exn name (lambda (x) #t) (lambda () (eval (compile-term (parse-expression expression))))))
  
  ; compiler-test-suite :: schemeunit-test-suite
  (define compiler-test-suite
    (test-suite "compiler"
                (test-case-e "application1" "(\\x -> x) 1" 1)
                (test-case-e "application2" "(\\x y -> x) 1 2" 1)
                (test-case-x "application3" "1 2")
                (test-case-x "application4" "(\\x -> x) 1 2")
                (test-case-e "character1" "'a'" #\a)
                (test-case-e "character2" "'Z'" #\Z)
                (test-case-e "float1" "0.0" 0.0)
                (test-case-e "float2" "1.1" 1.1)
                (test-case-e "float3" "123456789.987654321" 123456789.987654321)
                (test-case-p "function1" "\\x -> x" (lambda (x) (equal? (x (delay 1)) 1)))
                (test-case-p "function2" "\\x y -> x" (lambda (x) (equal? ((x (delay 1)) (delay 2)) 1)))
                ;(test-case-he "haskell1" "Bool" "True" #t)
                (test-case-he "haskell2" "Char" "'a'" #\a)
                (test-case-he "haskell3" "Float" "1.2" 1.2)
                (test-case-hp "haskell4" "Int -> Int" "\\x -> x" (lambda (x) 'TODO))
                (test-case-he "haskell5" "Int" "1" 1)
                (test-case-he "haskell6" "[Int]" "[]" null)
                (test-case-hp "haskell7" "[Int]" "[1]" (lambda (x) (and (pair? x)
                                                                        (equal? (force (car x)) 1)
                                                                        (equal? (force (cdr x)) null))))
                (test-case-hp "haskell8" "[Int]" "[1, 2]" (lambda (x) (and (pair? x)
                                                                           (equal? (force (car x)) 1)
                                                                           (pair? (force (cdr x)))
                                                                           (equal? (force (car (force (cdr x)))) 2)
                                                                           (equal? (force (cdr (force (cdr x)))) null))))
                (test-case-hp "haskell9" "(Int, Int)" "(1, 2)" (lambda (x) (and (vector? x)
                                                                                (immutable? x)
                                                                                (equal? (force (vector-ref x 0)) 1)
                                                                                (equal? (force (vector-ref x 1)) 2))))
                (test-case-hp "haskell10" "(Int, Int, Int)" "(1, 2, 3)" (lambda (x) (and (vector? x)
                                                                                        (immutable? x)
                                                                                        (equal? (force (vector-ref x 0)) 1)
                                                                                        (equal? (force (vector-ref x 1)) 2)
                                                                                        (equal? (force (vector-ref x 2)) 3))))
                (test-case-x "identifier1" "x")
                #;(test-case-e "if1" "if true then 1 else 2" 1)
                (test-case-e "integer1" "0" 0)
                (test-case-e "integer2" "1" 1)
                (test-case-e "integer3" "123456789" 123456789)
                (test-case-e "let1" "let { i = 1 } in 2" 2)
                (test-case-e "let2" "let { i = 1 ; j = 2 } in 3" 3)
                (test-case-e "let3" "let { i = 1 } in i" 1)
                (test-case-e "let4" "let { i = 1 ; j = i } in j" 1)
                (test-case-e "let5" "let { i = j ; j = 1 } in i" 1)
                (test-case-p "let6" "let { i x = 1 } in i" (lambda (x) (equal? (x (delay 2)) 1)))
                (test-case-p "let7" "let { i x = x } in i" (lambda (x) (equal? (x (delay 1)) 1)))
                (test-case-p "let8" "let { i x y = x } in i" (lambda (x) (equal? ((x (delay 1)) (delay 2)) 1)))
                #;(test-case-p "let9" "let { i = i } in i" (lambda (x) (equal? x (force x))))
                #;(test-case-p "let10" "let { i = j ; j = i } in (i, j)" (lambda (x) (and (equal? (vector-ref x 0) (force (vector-ref x 1)))
                                                                                          (equal? (force (vector-ref x 0)) (vector-ref x 1)))))
                (test-case-e "list1" "[]" null)
                (test-case-p "list2" "[1]" (lambda (x) (and (pair? x)
                                                            (equal? (force (car x)) 1)
                                                            (equal? (force (cdr x)) null))))
                (test-case-p "list3" "[1, 2]" (lambda (x) (and (pair? x)
                                                               (equal? (force (car x)) 1)
                                                               (pair? (force (cdr x)))
                                                               (equal? (force (car (force (cdr x)))) 2)
                                                               (equal? (force (cdr (force (cdr x)))) null))))
                (test-case-p "tuple1" "(1, 2)" (lambda (x) (and (vector? x)
                                                                (immutable? x)
                                                                (equal? (force (vector-ref x 0)) 1)
                                                                (equal? (force (vector-ref x 1)) 2))))
                (test-case-p "tuple2" "(1, 2, 3)" (lambda (x) (and (vector? x)
                                                                   (immutable? x)
                                                                   (equal? (force (vector-ref x 0)) 1)
                                                                   (equal? (force (vector-ref x 1)) 2)
                                                                   (equal? (force (vector-ref x 2)) 3))))
                (test-case-p "tuplecon1" "(,)" (lambda (x) (let ((t ((x (delay 1)) (delay 2))))
                                                             (and (vector? t)
                                                                  (immutable? t)
                                                                  (equal? (force (vector-ref t 0)) 1)
                                                                  (equal? (force (vector-ref t 1)) 2)))))
                (test-case-p "tuplecon2" "(,,)" (lambda (x) (let ((t (((x (delay 1)) (delay 2)) (delay 3))))
                                                              (and (vector? t)
                                                                   (immutable? t)
                                                                   (equal? (force (vector-ref t 0)) 1)
                                                                   (equal? (force (vector-ref t 1)) 2)
                                                                   (equal? (force (vector-ref t 2)) 3)))))))
  
  ; parse-expression :: string -> term
  (define (parse-expression expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (test-expression-parser (lambda () (language-lexer port)))))
  
  ; parse-type :: string -> type
  (define (parse-type type)
    (let ((port (open-input-string type)))
      (port-count-lines! port)
      (normalize-type-variables (map-type (lambda (x) (if (type-constructor? x) (translate-type-constructor x) x))
                                          (test-type-parser (lambda () (language-lexer port)))))))
  
  ; run-tests :: [string]
  (define (run-tests)
    (define (results x y)
      (cond ((test-failure? x) (cons (test-result-test-case-name x) y))
            ((test-error? x) (cons (test-result-test-case-name x) y))
            (else y)))
    (fold-test-results results null compiler-test-suite))
  
  ; strict-equal? :: 'a 'a -> boolean
  (define (strict-equal? x y)
    (cond ((promise? x) (strict-equal? (force x) y))
          ((promise? y) (strict-equal? x (force y)))
          ((and (pair? x) (pair? y)) (and (strict-equal? (car x) (car y)) (strict-equal? (cdr x) (cdr y))))
          ((and (vector? x) (vector? y) (equal? (vector-length x) (vector-length y)))
           (foldl (lambda (x y) (and y (strict-equal? (list-ref x 0) (list-ref x 1)))) #t (zip (vector->list x) (vector->list y))))
          (else (equal? x y))))
  
  ; test-expression-parser :: parser
  (define test-expression-parser (expression-parser "test"))
  
  ; test-type-parser :: parser
  (define test-type-parser (type-parser "test")))