(module ParsersTest mzscheme
  (require (lib "Parsers.ss" "sham" "haskell")
           (lib "HaskellSyntax.ss" "sham" "haskell")
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
                              "x y"
                              (make-Application (make-Variable "x") (make-Variable "y")))
                (test-case-ee "ap2"
                              "x y z"
                              (make-Application (make-Application (make-Variable "x")
                                                                  (make-Variable "y"))
                                                (make-Variable "z")))
                (test-case-ee "ch1"
                              "'a'"
                              (make-Character "a"))
                (test-case-de "da1"
                              "data A = B"
                              (make-Data "A" (list (make-Constructor "B" null))))
                (test-case-de "da2"
                              "data A = B {}"
                              (make-Data "A" (list (make-Constructor "B" null))))
                (test-case-de "da3"
                              "data A = B | C"
                              (make-Data "A" (list (make-Constructor "B" null)
                                                   (make-Constructor "C" null))))
                (test-case-de "da4"
                              "data A = B { c :: A }"
                              (make-Data "A" (list (make-Constructor "B" (list (make-Field "c" (make-TypeConstructor "A")))))))
                (test-case-de "da5"
                              "data A = B { c :: A, d :: A }"
                              (make-Data "A" (list (make-Constructor "B" (list (make-Field "c" (make-TypeConstructor "A"))
                                                                               (make-Field "d" (make-TypeConstructor "A")))))))
                (test-case-de "da6"
                              "data A = B { c, d :: A }"
                              (make-Data "A" (list (make-Constructor "B" (list (make-Field "c" (make-TypeConstructor "A"))
                                                                               (make-Field "d" (make-TypeConstructor "A")))))))
                (test-case-de "de1"
                              "x = 1"
                              (make-Declaration (make-LHS "x" null) (make-Integer "1")))
                (test-case-de "de2"
                              "x y = 1"
                              (make-Declaration (make-LHS "x" (list "y")) (make-Integer "1")))
                (test-case-ee "fl1"
                              "1.2"
                              (make-Float "1.2"))
                (test-case-ee "fu1"
                              "\\x -> 1"
                              (make-Function (list "x")
                                             (make-Integer "1")))
                (test-case-ee "fu2"
                              "\\x -> \\y -> 1"
                              (make-Function (list "x")
                                             (make-Function (list "y")
                                                            (make-Integer "1"))))
                (test-case-ee "fu3"
                              "\\x y -> 1"
                              (make-Function (list "x" "y")
                                             (make-Integer "1")))
                (test-case-ee "id1"
                              "x"
                              (make-Variable "x"))
                (test-case-ee "id2"
                              "(x)"
                              (make-Variable "x"))
                (test-case-ee "id3"
                              "(:)"
                              (make-Variable ":"))
                (test-case-ee "if1"
                              "if x then 1 else 2"
                              (make-If (make-Variable "x")
                                       (make-Integer "1")
                                       (make-Integer "2")))
                (test-case-ee "in1"
                              "1"
                              (make-Integer "1"))
                (test-case-ee "le1"
                              "let {} in 1"
                              (make-Let null (make-Integer "1")))
                (test-case-ee "le2"
                              "let { x = 1 } in x"
                              (make-Let (list (make-Declaration (make-LHS "x" null) (make-Integer "1")))
                                        (make-Variable "x")))
                (test-case-ee "le3"
                              "let { x = 1 ; y = 1} in x"
                              (make-Let (list (make-Declaration (make-LHS "x" null) (make-Integer "1"))
                                              (make-Declaration (make-LHS "y" null) (make-Integer "1")))
                                        (make-Variable "x")))
                (test-case-ee "li1"
                              "[]"
                              (make-ListConstructor))
                (test-case-ee "li2"
                              "[1]"
                              (make-List (list (make-Integer "1"))))
                (test-case-ee "li3"
                              "[1, 2]"
                              (make-List (list (make-Integer "1")
                                               (make-Integer "2"))))
                (test-case-ee "ml1"
                              ":ml Int \"x\""
                              (make-ML (make-TypeConstructor "Int")
                                       "x"))
                (test-case-me "mo1"
                              "module M where {}"
                              (make-Module "M" (make-Body null null)))
                (test-case-me "mo2"
                              "{}"
                              (make-Module "none" (make-Body null null)))
                (test-case-me "mo3"
                              "{ x = 1 }"
                              (make-Module "none" (make-Body null (list (make-Declaration (make-LHS "x" null) (make-Integer "1"))))))
                (test-case-me "mo4"
                              "{ x = 1 ; data A = B }"
                              (make-Module "none" (make-Body null (list (make-Declaration (make-LHS "x" null) (make-Integer "1"))
                                                                        (make-Data "A" (list (make-Constructor "B" null)))))))
                (test-case-me "mo5"
                              "{ data A = B ; x = 1 }"
                              (make-Module "none" (make-Body null (list (make-Data "A" (list (make-Constructor "B" null)))
                                                                        (make-Declaration (make-LHS "x" null) (make-Integer "1"))))))
                (test-case-ee "sc1"
                              ":scheme A \"x\""
                              (make-Scheme (make-TypeConstructor "A") "x"))
                (test-case-ee "tu1"
                              "(1, 2)"
                              (make-Tuple (list (make-Integer "1") (make-Integer "2"))))
                (test-case-ee "tu1"
                              "(1, 2, 3)"
                              (make-Tuple (list (make-Integer "1") (make-Integer "2") (make-Integer "3"))))
                (test-case-ee "tc1"
                              "(,)"
                              (make-TupleConstructor 2))
                (test-case-ee "tc2"
                              "(,,)"
                              (make-TupleConstructor 3))
                (test-case-te "ty1"
                              "A"
                              (make-TypeConstructor "A"))
                (test-case-te "ty2"
                              "A -> B"
                              (make-FunctionType (make-TypeConstructor "A") (make-TypeConstructor "B")))
                (test-case-te "ty3"
                              "[A]"
                              (make-ListType (make-TypeConstructor "A")))
                (test-case-te "ty4"
                              "(A, B)"
                              (make-TupleType (list (make-TypeConstructor "A")
                                                    (make-TypeConstructor "B"))))
                (test-case-te "ty5"
                              "a"
                              (make-TypeVariable "a"))
                (test-case-te "ty6"
                              "()"
                              (make-UnitType))
                (test-case-ee "un1"
                              "()"
                              (make-UnitConstructor))))
  
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