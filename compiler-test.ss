(module compiler-test mzscheme
  (require (lib "compiler.ss" "haskell")
           (lib "reader.ss" "haskell")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide run-tests)
  
  ; run-tests :: [string]
  (define (run-tests)
    (let* ((test-suites (test-suite "all"
                                    application-test-suite
                                    character-test-suite
                                    float-test-suite
                                    function-test-suite
                                    identifier-test-suite
                                    ;if-test-suite
                                    integer-test-suite
                                    let-test-suite
                                    list-test-suite
                                    tuple-test-suite
                                    tuplecon-test-suite))
           (results (lambda (x y)
                      (cond ((test-failure? x) (cons (test-result-test-case-name x) y))
                            ((test-error? x) (cons (test-result-test-case-name x) y))
                            (else y)))))
      (fold-test-results results null test-suites)))
  
  (define test-expression-parser (expression-parser "test"))
  
  ; parse-expression :: string -> term
  (define (parse-expression expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (test-expression-parser (lambda () (language-lexer port)))))
  
  ; test-case-success :: string -> string -> datum -> test-case
  (define (test-case-success name expression result)
    (test-equal? name (compile-term (parse-expression expression)) result))
  
  (define application-test-suite
    (test-suite "application"
                (test-case-success "app1" "x 4" `((force x) (delay 4)))
                (test-case-success "app2" "x 5 6" `(((force x) (delay 5)) (delay 6)))))
  
  (define character-test-suite
    (test-suite "character"
                (test-case-success "char1" "'c'" `#\c)))
  
  (define float-test-suite
    (test-suite "float"
                (test-case-success "float1" "1.2" `1.2)))
  
  (define function-test-suite
    (test-suite "function"
                (test-case-success "fun" "\\x -> x" `(lambda (haskell:x) (force haskell:x)))
                (test-case-success "fun" "\\x y -> x" `(lambda (haskell:x) (lambda (haskell:y) (force haskell:x))))))
  
  (define identifier-test-suite
    (test-suite "identifier"
                (test-case-success "id1" "x" `(force haskell:x))
                (test-case-success "id2" "(:)" `(force prelude:list-cons))))
  
  (define if-test-suite
    (test-suite "if"
                (test-case-success "if1" "if true then 1 else 2" `(if (force haskell:true) 1 2))))
  
  (define integer-test-suite
    (test-suite "integer"
                (test-case-success "int1" "1" `1)))
  
  (define let-test-suite
    (test-suite "let"
                (test-case-success "let1" "let { x = 1 } in 2" `(letrec ((x (delay 1))) 2))
                (test-case-success "let2" "let { x = 1 ; y = 2 } in 3" `(letrec ((x (delay 1)) (y (delay 2))) 3))
                (test-case-success "let3" "let { x y = 1 } in 2" `(letrec ((x (delay (lambda (y) 1)))) 2))))
  
  (define list-test-suite
    (test-suite "list"
                (test-case-success "list1" "[]" `())
                (test-case-success "list2" "[1]" `(cons (delay 1) (delay ())))
                (test-case-success "list3" "[1, 2]" `(cons (delay 1) (delay (cons (delay 2) (delay ())))))))
  
  (define tuple-test-suite
    (test-suite "tuple"
                (test-case-success "tup1" "(1, 'a')" `(vector-immutable 1 #\a))))
  
  (define tuplecon-test-suite
    (test-suite "tuplecon"
                (test-case-success "tupcon1" "(,)" `(lambda (x1) (lambda (x2) (vector-immutable x1 x2))))
                (test-case-success "tupcon2" "(,,)" `(lambda (x1) (lambda (x2) (lambda (x3) (vector-immutable x1 x2 x3))))))))