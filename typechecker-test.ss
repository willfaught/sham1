(module typechecker-test mzscheme
  (require (lib "match.ss")
           (lib "reader.ss" "haskell")
           (lib "typechecker.ss" "haskell")
           (lib "types.ss" "haskell")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide run-tests)
  
  (define test-expression-parser (expression-parser "test"))
  
  (define test-type-parser (type-parser "test"))
  
  (define (parse-expression expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (test-expression-parser (lambda () (language-lexer port)))))
  
  (define (parse-type type)
    (let ((port (open-input-string type)))
      (port-count-lines! port)
      (normalize-type-variables (map-type (lambda (x) (if (type-constructor? x) (translate-type-constructor x) x))
                                          (test-type-parser (lambda () (language-lexer port)))))))
  
  (define (test-case-success name expression type)
    (test-equal? name (reconstruct-type (parse-expression expression)) (parse-type type)))
  
  (define (test-case-error name expression)
    (test-exn name (lambda (x) #t) (lambda () (reconstruct-type (parse-expression expression)))))
  
  (define (run-tests)
    (let* ((test-suites (test-suite "all"
                                    boolean-test-suite
                                    character-test-suite
                                    integer-test-suite
                                    float-test-suite
                                    list-test-suite
                                    tuple-test-suite
                                    tuplecon-test-suite
                                    let-test-suite))
           (results (lambda (x y)
                      (cond ((test-failure? x) (cons (test-result-test-case-name x) y))
                            ((test-error? x) (cons (test-result-test-case-name x) y))
                            (else y)))))
      (fold-test-results results null test-suites)))
  
  (define boolean-test-suite
    (test-suite "boolean"
                (test-case-success "bool1" "True" "Bool")
                (test-case-success "bool2" "False" "Bool")))
  
  (define character-test-suite
    (test-suite "character"
                (test-case-success "char1" "'a'" "Char")))
  
  (define integer-test-suite
    (test-suite "integer"
                (test-case-success "int1" "1" "Int")))
  
  (define float-test-suite
    (test-suite "float"
                (test-case-success "float1" "1.2" "Float")))
  
  (define list-test-suite
    (test-suite "list"
                (test-case-success "list1" "[]" "[t]")
                (test-case-success "list2" "['a']" "[Char]")
                (test-case-success "list3" "[1.2, 3.4]" "[Float]")))
  
  (define tuple-test-suite
    (test-suite "tuple"
                (test-case-success "tuple1" "('a', 1.2)" "(Char, Float)")
                (test-case-success "tuple2" "('a', 1.2, 3)" "(Char, Float, Int)")))
  
  (define tuplecon-test-suite
    (test-suite "tuplecon"
                (test-case-success "tupcon1" "(,)" "t -> t1 -> (t, t1)")
                (test-case-success "tupcon2" "(,,)" "t -> t1 -> t2 -> (t, t1, t2)")))
  
  (define -test-suite
    (test-suite ""
                (test-case-success "" "" "")))
  
  (define -test-suite
    (test-suite ""
                (test-case-success "" "" "")))
  
  (define let-test-suite
    (test-suite "let"
                (test-case-error "let1" "let { a x = x ; b = a 2 ; c = a 'b' } in a")))
  
  #;(define tests
      (list (make-test "let-term 1"
                       (make-let-term (list (make-declaration-term (list "a")
                                                                   (make-character-term "a")))
                                      (make-float-term "1.2"))
                       (make-float-type))
            (make-test "let-term 2"
                       (make-let-term (list (make-declaration-term (list "a")
                                                                   (make-character-term "a")))
                                      (make-identifier-term "a"))
                       (make-character-type))
            (make-test "let-term 3"
                       (make-let-term (list (make-declaration-term (list "a")
                                                                   (make-identifier-term "a")))
                                      (make-identifier-term "a"))
                       (make-type-variable "t2"))
            (make-test "let-term 4"
                       (make-let-term (list (make-declaration-term (list "a")
                                                                   (make-character-term "a"))
                                            (make-declaration-term (list "b")
                                                                   (make-identifier-term "a")))
                                      (make-identifier-term "b"))
                       (make-character-type))
            (make-test "let-term 5"
                       (make-let-term (list (make-declaration-term (list "a")
                                                                   (make-identifier-term "b"))
                                            (make-declaration-term (list "b")
                                                                   (make-character-term "a")))
                                      (make-identifier-term "a"))
                       (make-character-type))
            (make-test "let-term 6"
                       (make-let-term (list (make-declaration-term (list "a" "x")
                                                                   (make-character-term "a")))
                                      (make-identifier-term "a"))
                       (make-function-type (list (make-type-variable "t3")
                                                 (make-character-type))))
            (make-test "let-term 7"
                       (make-let-term (list (make-declaration-term (list "a" "x")
                                                                   (make-identifier-term "x")))
                                      (make-identifier-term "a"))
                       (make-function-type (list (make-type-variable "t3")
                                                 (make-type-variable "t3"))))
            (make-test "let-term 8"
                       (make-let-term (list (make-declaration-term (list "a" "x" "y")
                                                                   (make-identifier-term "y")))
                                      (make-identifier-term "a"))
                       (make-function-type (list (make-type-variable "t4")
                                                 (make-type-variable "t5")
                                                 (make-type-variable "t5"))))
            (make-test "let-term 9"
                       (make-let-term (list (make-declaration-term (list "a" "x")
                                                                   (make-identifier-term "x"))
                                            (make-declaration-term (list "b")
                                                                   (make-application-term (make-identifier-term "a")
                                                                                          (list (make-character-term "a"))))
                                            (make-declaration-term (list "c")
                                                                   (make-application-term (make-identifier-term "a")
                                                                                          (list (make-character-term "b")))))
                                      (make-identifier-term "a"))
                       (make-function-type (list (make-character-type)
                                                 (make-character-type))))
            (make-test "let-term 10"
                       (make-let-term (list (make-declaration-term (list "a" "x")
                                                                   (make-integer-term "1")))
                                      (make-application-term (make-identifier-term "a")
                                                             (list (make-character-term "a"))))
                       (make-integer-type))
            (make-test "let-term 11"
                       (make-let-term (list (make-declaration-term (list "a" "x")
                                                                   (make-identifier-term "x")))
                                      (make-application-term (make-identifier-term "a")
                                                             (list (make-character-term "a"))))
                       (make-character-type))
            (make-test "let-term 12"
                       (make-let-term (list (make-declaration-term (list "a" "x")
                                                                   (make-identifier-term "x")))
                                      (make-let-term (list (make-declaration-term (list "b") (make-application-term (make-identifier-term "a")
                                                                                                                    (list (make-character-term "b"))))
                                                           (make-declaration-term (list "c") (make-application-term (make-identifier-term "a")
                                                                                                                    (list (make-float-term "1.2")))))
                                                     (make-identifier-term "a")))
                       (make-function-type (list (make-type-variable "t9")
                                                 (make-type-variable "t9"))))
            (make-test "let-term 13"
                       (make-let-term (list (make-declaration-term (list "a" "x")
                                                                   (make-identifier-term "x")))
                                      (make-let-term (list (make-declaration-term (list "b") (make-application-term (make-identifier-term "a")
                                                                                                                    (list (make-character-term "b"))))
                                                           (make-declaration-term (list "c") (make-application-term (make-identifier-term "a")
                                                                                                                    (list (make-float-term "1.2")))))
                                                     (make-identifier-term "b")))
                       (make-character-type))
            (make-test "function-term 1"
                       (make-function-term (list "x")
                                           (make-character-term "a"))
                       (make-function-type (list (make-type-variable "t1")
                                                 (make-character-type))))
            (make-test "function-term 2"
                       (make-function-term (list "x")
                                           (make-identifier-term "x"))
                       (make-function-type (list (make-type-variable "t1")
                                                 (make-type-variable "t1"))))
            (make-test "function-term 3"
                       (make-function-term (list "x" "y")
                                           (make-identifier-term "y"))
                       (make-function-type (list (make-type-variable "t1")
                                                 (make-type-variable "t2")
                                                 (make-type-variable "t2"))))
            (make-test "function-term 4"
                       (make-function-term (list "x" "y")
                                           (make-tuple-term (list (make-identifier-term "x")
                                                                  (make-identifier-term "y"))))
                       (make-function-type (list (make-type-variable "t1")
                                                 (make-type-variable "t2")
                                                 (make-tuple-type (list (make-type-variable "t1") (make-type-variable "t2"))))))
            (make-test "application-term 1"
                       (make-application-term (make-function-term (list "x")
                                                                  (make-character-term "a"))
                                              (list (make-float-term "1.2")))
                       (make-character-type))
            (make-test "application-term 2"
                       (make-application-term (make-function-term (list "x")
                                                                  (make-identifier-term "x"))
                                              (list (make-character-term "a")))
                       (make-character-type))
            (make-test "application-term 3"
                       (make-application-term (make-function-term (list "x" "y")
                                                                  (make-identifier-term "y"))
                                              (list (make-character-term "a")
                                                    (make-float-term "1.2")))
                       (make-float-type))
            (make-test "application-term 4"
                       (make-application-term (make-function-term (list "x" "y")
                                                                  (make-tuple-term (list (make-identifier-term "x")
                                                                                         (make-identifier-term "y"))))
                                              (list (make-character-term "b")
                                                    (make-float-term "1.2")))
                       (make-tuple-type (list (make-character-type)
                                              (make-float-type))))
            )))