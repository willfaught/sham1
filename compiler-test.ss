(module compiler-test mzscheme
  (require (lib "compiler.ss" "haskell")
           (lib "reader.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "types.ss" "haskell")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide run-tests)

  ; test-case-e :: string string 'a -> schemeunit-test-case
  (define (test-case-e name expression value)
    (test-equal? name (eval (compile-term (parse-expression expression))) value))
  
  ; test-case-h :: string string datum -> schemeunit-test-case
  (define (test-case-h name type result)
    (test-equal? name (compile-term (make-haskell-term (parse-type type) `x)) result))
  
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
                ;(test-case-h "has1" "Bool" `x)
                ;(test-case-h "has2" "Char" `x)
                ;(test-case-h "has3" "Float" `x)
                ;(test-case-h "has4" "Int -> Int" `x)
                ;(test-case-h "has1" "Int" `x)
                ;(test-case-h "has1" "[Int]" `x)
                ;(test-case-h "has1" "(Int, Int)" `x)
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
  
  ; test-expression-parser :: parser
  (define test-expression-parser (expression-parser "test"))
  
  ; test-type-parser :: parser
  (define test-type-parser (type-parser "test")))