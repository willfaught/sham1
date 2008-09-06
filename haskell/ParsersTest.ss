(module ParsersTest mzscheme
  (require (lib "Parsers.ss" "sham" "haskell")
           (lib "HaskellSyntax.ss" "sham" "haskell")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide runTests)
  
  ; tcde :: string string term -> schemeunit-test-case
  (define (tcde name expression value)
    (test-equal? name (parseD expression) value))
  
  ; tcee :: string string term -> schemeunit-test-case
  (define (tcee name expression value)
    (test-equal? name (parseE expression) value))
  
  ; tcme :: string string term -> schemeunit-test-case
  (define (tcme name expression value)
    (test-equal? name (parseM expression) value))
  
  ; tcte :: string string term -> schemeunit-test-case
  (define (tcte name expression value)
    (test-equal? name (parseT expression) value))
  
  ; tests :: schemeunit-test-suite
  (define tests
    (test-suite "parsers"
                (tcee "ap1"
                      "x y"
                      (make-Application (make-Variable "x") (make-Variable "y")))
                (tcee "ap2"
                      "x y z"
                      (make-Application (make-Application (make-Variable "x")
                                                          (make-Variable "y"))
                                        (make-Variable "z")))
                (tcee "ch1"
                      "'a'"
                      (make-Character "a"))
                (tcde "da1"
                      "data A = B"
                      (make-Data "A" (list (make-Constructor "B" null))))
                (tcde "da2"
                      "data A = B {}"
                      (make-Data "A" (list (make-Constructor "B" null))))
                (tcde "da3"
                      "data A = B | C"
                      (make-Data "A" (list (make-Constructor "B" null)
                                           (make-Constructor "C" null))))
                (tcde "da4"
                      "data A = B { c :: A }"
                      (make-Data "A" (list (make-Constructor "B" (list (make-Field "c" (make-TypeConstructor "A")))))))
                (tcde "da5"
                      "data A = B { c :: A, d :: A }"
                      (make-Data "A" (list (make-Constructor "B" (list (make-Field "c" (make-TypeConstructor "A"))
                                                                       (make-Field "d" (make-TypeConstructor "A")))))))
                (tcde "da6"
                      "data A = B { c, d :: A }"
                      (make-Data "A" (list (make-Constructor "B" (list (make-Field "c" (make-TypeConstructor "A"))
                                                                       (make-Field "d" (make-TypeConstructor "A")))))))
                (tcde "de1"
                      "x = 1"
                      (make-Declaration (make-LHS "x" null) (make-Integer "1")))
                (tcde "de2"
                      "x y = 1"
                      (make-Declaration (make-LHS "x" (list "y")) (make-Integer "1")))
                (tcee "fl1"
                      "1.2"
                      (make-Float "1.2"))
                (tcee "fu1"
                      "\\x -> 1"
                      (make-Function (list "x")
                                     (make-Integer "1")))
                (tcee "fu2"
                      "\\x -> \\y -> 1"
                      (make-Function (list "x")
                                     (make-Function (list "y")
                                                    (make-Integer "1"))))
                (tcee "fu3"
                      "\\x y -> 1"
                      (make-Function (list "x" "y")
                                     (make-Integer "1")))
                (tcee "id1"
                      "x"
                      (make-Variable "x"))
                (tcee "id2"
                      "(x)"
                      (make-Variable "x"))
                (tcee "id3"
                      "(:)"
                      (make-Variable ":"))
                (tcee "if1"
                      "if x then 1 else 2"
                      (make-If (make-Variable "x")
                               (make-Integer "1")
                               (make-Integer "2")))
                (tcee "in1"
                      "1"
                      (make-Integer "1"))
                (tcee "le1"
                      "let {} in 1"
                      (make-Let null (make-Integer "1")))
                (tcee "le2"
                      "let { x = 1 } in x"
                      (make-Let (list (make-Declaration (make-LHS "x" null) (make-Integer "1")))
                                (make-Variable "x")))
                (tcee "le3"
                      "let { x = 1 ; y = 1} in x"
                      (make-Let (list (make-Declaration (make-LHS "x" null) (make-Integer "1"))
                                      (make-Declaration (make-LHS "y" null) (make-Integer "1")))
                                (make-Variable "x")))
                (tcee "li1"
                      "[]"
                      (make-ListConstructor))
                (tcee "li2"
                      "[1]"
                      (make-List (list (make-Integer "1"))))
                (tcee "li3"
                      "[1, 2]"
                      (make-List (list (make-Integer "1")
                                       (make-Integer "2"))))
                (tcee "ml1"
                      ":ml Int \"x\""
                      (make-ML (make-TypeConstructor "Int")
                               "x"))
                (tcme "mo1"
                      "module M where {}"
                      (make-Module "M" (make-Body null null)))
                (tcme "mo2"
                      "{}"
                      (make-Module "none" (make-Body null null)))
                (tcme "mo3"
                      "{ x = 1 }"
                      (make-Module "none" (make-Body null (list (make-Declaration (make-LHS "x" null) (make-Integer "1"))))))
                (tcme "mo4"
                      "{ x = 1 ; data A = B }"
                      (make-Module "none" (make-Body null (list (make-Declaration (make-LHS "x" null) (make-Integer "1"))
                                                                (make-Data "A" (list (make-Constructor "B" null)))))))
                (tcme "mo5"
                      "{ data A = B ; x = 1 }"
                      (make-Module "none" (make-Body null (list (make-Data "A" (list (make-Constructor "B" null)))
                                                                (make-Declaration (make-LHS "x" null) (make-Integer "1"))))))
                (tcee "sc1"
                      ":scheme A \"x\""
                      (make-Scheme (make-TypeConstructor "A") "x"))
                (tcee "tu1"
                      "(1, 2)"
                      (make-Tuple (list (make-Integer "1") (make-Integer "2"))))
                (tcee "tu1"
                      "(1, 2, 3)"
                      (make-Tuple (list (make-Integer "1") (make-Integer "2") (make-Integer "3"))))
                (tcee "tc1"
                      "(,)"
                      (make-TupleConstructor 2))
                (tcee "tc2"
                      "(,,)"
                      (make-TupleConstructor 3))
                (tcte "ty1"
                      "A"
                      (make-TypeConstructor "A"))
                (tcte "ty2"
                      "A -> B"
                      (make-FunctionType (make-TypeConstructor "A") (make-TypeConstructor "B")))
                (tcte "ty3"
                      "[A]"
                      (make-ListType (make-TypeConstructor "A")))
                (tcte "ty4"
                      "(A, B)"
                      (make-TupleType (list (make-TypeConstructor "A")
                                            (make-TypeConstructor "B"))))
                (tcte "ty5"
                      "a"
                      (make-TypeVariable "a"))
                (tcte "ty6"
                      "()"
                      (make-UnitType))
                (tcee "un1"
                      "()"
                      (make-UnitConstructor))))
  
  ; parseD :: string -> HaskellSyntax
  (define (parseD expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (dParser (lambda () (language-lexer port)))))
  
  ; parseE :: string -> HaskellSyntax
  (define (parseE expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (eParser (lambda () (language-lexer port)))))
  
  ; parseM :: string -> HaskellSyntax
  (define (parseM expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (mParser (lambda () (language-lexer port)))))
  
  ; parseT :: string -> HaskellSyntax
  (define (parseT expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (tParser (lambda () (language-lexer port)))))
  
  ; runTests :: [string]
  (define (runTests)
    (define (results x y)
      (cond ((test-failure? x) (cons (test-result-test-case-name x) y))
            ((test-error? x) (cons (test-result-test-case-name x) y))
            (else y)))
    (fold-test-results results null tests))
  
  ; dParser :: parser
  (define dParser (declaration-parser "test"))
  
  ; eParser :: parser
  (define eParser (expression-parser "test"))
  
  ; mParser :: parser
  (define mParser (module-parser "test"))
  
  ; tParser :: parser
  (define tParser (type-parser "test")))