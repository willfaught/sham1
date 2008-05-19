(module parsers-test mzscheme
  (require (lib "parsers.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "types.ss" "haskell")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide run-tests)
  
  ; test-case-de :: string string term -> schemeunit-test-case
  (define (test-case-de name expression value)
    (test-equal? name (parse-declaration expression) value))
  
  ; test-case-ee :: string string term -> schemeunit-test-case
  (define (test-case-ee name expression value)
    (test-equal? name (parse-expression expression) value))
  
  ; test-case-me :: string string term -> schemeunit-test-case
  (define (test-case-me name expression value)
    (test-equal? name (parse-module expression) value))
  
  ; test-case-te :: string string term -> schemeunit-test-case
  (define (test-case-te name expression value)
    (test-equal? name (parse-type expression) value))
  
  ; parsers-test-suite :: schemeunit-test-suite
  (define parsers-test-suite
    (test-suite "parsers"
                (test-case-ee "ap1"
                              "x 1"
                              (make-application-term (make-identifier-term "x")
                                                     (list (make-integer-term "1"))))
                (test-case-ee "ap2"
                              "x 1 2"
                              (make-application-term (make-identifier-term "x")
                                                     (list (make-integer-term "1")
                                                           (make-integer-term "2"))))
                (test-case-ee "ch1"
                              "'a'"
                              (make-character-term "a"))
                (test-case-ee "ch2"
                              "'Z'"
                              (make-character-term "Z"))
                (test-case-de "da1"
                              "data A = B"
                              (make-data-term "A" (list (make-constructor-term "B" null))))
                (test-case-de "da2"
                              "data A = B {}"
                              (make-data-term "A" (list (make-constructor-term "B" null))))
                (test-case-de "da3"
                              "data A = B | C"
                              (make-data-term "A" (list (make-constructor-term "B" null)
                                                        (make-constructor-term "C" null))))
                (test-case-de "da4"
                              "data A = B { c :: A }"
                              (make-data-term "A" (list (make-constructor-term "B" (list (make-field-term "c" (make-type-constructor "A")))))))
                (test-case-de "da5"
                              "data A = B { c :: A, d :: A }"
                              (make-data-term "A" (list (make-constructor-term "B" (list (make-field-term "c" (make-type-constructor "A"))
                                                                                         (make-field-term "d" (make-type-constructor "A")))))))
                (test-case-de "da6"
                              "data A = B { c, d :: A }"
                              (make-data-term "A" (list (make-constructor-term "B" (list (make-field-term "c" (make-type-constructor "A"))
                                                                                         (make-field-term "d" (make-type-constructor "A")))))))
                (test-case-de "de1"
                              "x = 1"
                              (make-declaration-term (list "x") (make-integer-term "1")))
                (test-case-de "de2"
                              "x y = 1"
                              (make-declaration-term (list "x" "y") (make-integer-term "1")))
                (test-case-ee "fl1"
                              "1.2"
                              (make-float-term "1.2"))
                (test-case-ee "fu1"
                              "\\x -> 1"
                              (make-function-term (list "x")
                                                  (make-integer-term "1")))
                (test-case-ee "fu2"
                              "\\x -> \\y -> 1"
                              (make-function-term (list "x")
                                                  (make-function-term (list "y")
                                                                      (make-integer-term "1"))))
                (test-case-ee "fu3"
                              "\\x y -> 1"
                              (make-function-term (list "x" "y")
                                                  (make-integer-term "1")))
                (test-case-ee "id1"
                              "x"
                              (make-identifier-term "x"))
                (test-case-ee "id2"
                              "(x)"
                              (make-identifier-term "x"))
                (test-case-ee "id3"
                              "(:)"
                              (make-identifier-term ":"))
                (test-case-ee "id4"
                              "()"
                              (make-identifier-term "()"))
                (test-case-ee "if1"
                              "if x then 1 else 2"
                              (make-if-term (make-identifier-term "x")
                                            (make-integer-term "1")
                                            (make-integer-term "2")))
                (test-case-ee "in1"
                              "1"
                              (make-integer-term "1"))
                (test-case-ee "le1"
                              "let {} in 1"
                              (make-let-term null (make-integer-term "1")))
                (test-case-ee "le2"
                              "let { x = 1 } in x"
                              (make-let-term (list (make-declaration-term (list "x")
                                                                          (make-integer-term "1")))
                                             (make-identifier-term "x")))
                (test-case-ee "le3"
                              "let { x = 1 ; y = 1} in x"
                              (make-let-term (list (make-declaration-term (list "x")
                                                                          (make-integer-term "1"))
                                                   (make-declaration-term (list "y")
                                                                          (make-integer-term "1")))
                                             (make-identifier-term "x")))
                (test-case-ee "li1"
                              "[]"
                              (make-list-term null))
                (test-case-ee "li2"
                              "[1]"
                              (make-list-term (list (make-integer-term "1"))))
                (test-case-ee "li3"
                              "[1, 2]"
                              (make-list-term (list (make-integer-term "1")
                                                    (make-integer-term "2"))))
                (test-case-ee "ml1"
                              ":ml Int \"x\""
                              (make-ml-term (make-integer-type)
                                            "x"))
                (test-case-me "mo1"
                              "module M where {}"
                              (make-module-term "M" null null))
                (test-case-me "mo2"
                              "{}"
                              (make-module-term "none" null null))
                (test-case-me "mo3"
                              "{ x = 1 }"
                              (make-module-term "none"
                                                null
                                                (list (make-declaration-term (list "x")
                                                                             (make-integer-term "1")))))
                (test-case-me "mo4"
                              "{ x = 1 ; data A = B }"
                              (make-module-term "none"
                                                null
                                                (list (make-declaration-term (list "x")
                                                                             (make-integer-term "1"))
                                                      (make-data-term "A" (list (make-constructor-term "B" null))))))
                (test-case-me "mo5"
                              "{ data A = B ; x = 1 }"
                              (make-module-term "none"
                                                null
                                                (list (make-data-term "A" (list (make-constructor-term "B" null)))
                                                      (make-declaration-term (list "x")
                                                                             (make-integer-term "1")))))
                (test-case-ee "sc1"
                              ":scheme A \"x\""
                              (make-scheme-term (make-type-constructor "A")
                                                "x"))
                (test-case-ee "tu1"
                              "(1, 2)"
                              (make-tuple-term (list (make-integer-term "1")
                                                     (make-integer-term "2"))))
                (test-case-ee "tu1"
                              "(1, 2, 3)"
                              (make-tuple-term (list (make-integer-term "1")
                                                     (make-integer-term "2")
                                                     (make-integer-term "3"))))
                (test-case-ee "tc1"
                              "(,)"
                              (make-tuplecon-term 2))
                (test-case-ee "tc2"
                              "(,,)"
                              (make-tuplecon-term 3))
                (test-case-te "ty1"
                              "A"
                              (make-type-constructor "A"))
                (test-case-te "ty2"
                              "A -> B"
                              (make-function-type (make-type-constructor "A")
                                                  (make-type-constructor "B")))
                (test-case-te "ty3"
                              "[A]"
                              (make-list-type (make-type-constructor "A")))
                (test-case-te "ty4"
                              "(A, B)"
                              (make-tuple-type (list (make-type-constructor "A")
                                                     (make-type-constructor "B"))))
                (test-case-te "ty5"
                              "a"
                              (make-type-variable "a"))
                (test-case-te "ty6"
                              "()"
                              (make-type-constructor "()"))))
  
  ; parse-declaration :: string -> term
  (define (parse-declaration expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (test-declaration-parser (lambda () (language-lexer port)))))
  
  ; parse-expression :: string -> term
  (define (parse-expression expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (test-expression-parser (lambda () (language-lexer port)))))
  
  ; parse-module :: string -> term
  (define (parse-module expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (test-module-parser (lambda () (language-lexer port)))))
  
  ; parse-type :: string -> term
  (define (parse-type expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (test-type-parser (lambda () (language-lexer port)))))
  
  ; run-tests :: (string)
  (define (run-tests)
    (define (results x y)
      (cond ((test-failure? x) (cons (test-result-test-case-name x) y))
            ((test-error? x) (cons (test-result-test-case-name x) y))
            (else y)))
    (fold-test-results results null parsers-test-suite))
  
  ; test-declaration-parser :: parser
  (define test-declaration-parser (declaration-parser "test"))
  
  ; test-expression-parser :: parser
  (define test-expression-parser (expression-parser "test"))
  
  ; test-module-parser :: parser
  (define test-module-parser (module-parser "test"))
  
  ; test-type-parser :: parser
  (define test-type-parser (type-parser "test")))