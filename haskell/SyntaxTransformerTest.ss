(module SyntaxTransformerTest mzscheme
  (require (lib "Parsers.ss" "sham" "haskell")
           (prefix c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (lib "SyntaxTransformer.ss" "sham" "haskell")
           (prefix t/ (lib "Types.ss" "sham"))
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide runTests)
  
  ; tcde :: string string HaskellSyntax -> schemeunit-test-case
  (define (tcde name expression syntax)
    (test-equal? name (transformHC (parseD expression)) syntax))
  
  ; tcee :: string string HaskellSyntax -> schemeunit-test-case
  (define (tcee name expression syntax)
    (test-equal? name (transformHC (parseE expression)) syntax))
  
  ; tcme :: string string HaskellSyntax -> schemeunit-test-case
  (define (tcme name expression syntax)
    (test-equal? name (transformHC (parseM expression)) syntax))
  
  ; tcte :: string string HaskellSyntax -> schemeunit-test-case
  (define (tcte name expression syntax)
    (test-equal? name (transformHC (parseT expression)) syntax))
  
  ; tests :: schemeunit-test-suite
  (define tests
    (test-suite "HC syntax transformer"
                (tcee "ap1"
                      "x y"
                      (c/make-Application (c/make-Variable "x") (c/make-Variable "y")))
                (tcee "ap2"
                      "x y z"
                      (c/make-Application (c/make-Application (c/make-Variable "x") (c/make-Variable "y")) (c/make-Variable "z")))
                (tcee "ch1"
                      "'a'"
                      (c/make-Character "a"))
                (tcde "da1"
                      "data A = B"
                      (c/make-Data "A" (list (c/make-Constructor "B" null))))
                (tcde "da2"
                      "data A = B {}"
                      (c/make-Data "A" (list (c/make-Constructor "B" null))))
                (tcde "da3"
                      "data A = B { c :: D }"
                      (c/make-Data "A" (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "D")))))))
                (tcde "da4"
                      "data A = B { c, d :: E }"
                      (c/make-Data "A" (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "E"))
                                                                           (c/make-Field "d" (t/make-Constructor "E")))))))
                (tcde "da5"
                      "data A = B { c :: D, e :: F }"
                      (c/make-Data "A" (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "D"))
                                                                           (c/make-Field "e" (t/make-Constructor "F")))))))
                (tcde "da6"
                      "data A = B { c, d :: E, f, g :: H }"
                      (c/make-Data "A" (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "E"))
                                                                           (c/make-Field "d" (t/make-Constructor "E"))
                                                                           (c/make-Field "f" (t/make-Constructor "H"))
                                                                           (c/make-Field "g" (t/make-Constructor "H")))))))
                (tcde "da7"
                      "data A = B | C"
                      (c/make-Data "A" (list (c/make-Constructor "B" null) (c/make-Constructor "C" null))))
                (tcde "de1"
                      "x = 1"
                      (c/make-Declaration "x" (c/make-Integer "1")))
                (tcde "de2"
                      "x y = 1"
                      (c/make-Declaration "x" (c/make-Function "y" (c/make-Integer "1"))))
                (tcde "de3"
                      "x y z = 1"
                      (c/make-Declaration "x" (c/make-Function "y" (c/make-Function "z" (c/make-Integer "1")))))
                (tcee "fl1"
                      "1.2"
                      (c/make-Float "1.2"))
                (tcee "fu1"
                      "\\x -> 1"
                      (c/make-Function "x" (c/make-Integer "1")))
                (tcee "fu2"
                      "\\x y -> 1"
                      (c/make-Function "x" (c/make-Function "y" (c/make-Integer "1"))))
                (tcee "fu3"
                      "\\x -> \\y -> 1"
                      (c/make-Function "x" (c/make-Function "y" (c/make-Integer "1"))))
                (tcee "id1"
                      "x"
                      (c/make-Variable "x"))
                (tcee "id2"
                      "(x)"
                      (c/make-Variable "x"))
                (tcee "id3"
                      "(:)"
                      (c/make-Variable ":"))
                (tcee "if1"
                      "if x then y else z"
                      (c/make-If (c/make-Variable "x") (c/make-Variable "y") (c/make-Variable "z")))
                (tcee "in1"
                      "1"
                      (c/make-Integer "1"))
                (tcee "le1"
                      "let {} in x"
                      (c/make-Let null (c/make-Variable "x")))
                (tcee "le2"
                      "let { x = 1 } in x"
                      (c/make-Let (list (c/make-Declaration "x" (c/make-Integer "1"))) (c/make-Variable "x")))
                (tcee "le3"
                      "let { x = 1 ; y = 1} in x"
                      (c/make-Let (list (c/make-Declaration "x" (c/make-Integer "1"))
                                        (c/make-Declaration "y" (c/make-Integer "1")))
                                  (c/make-Variable "x")))
                (tcee "lc1"
                      "[]"
                      (c/make-ListConstructor))
                (tcee "li1"
                      "[1]"
                      (c/make-Application (c/make-Application (c/make-Variable ":") (c/make-Integer "1")) (c/make-ListConstructor)))
                (tcee "li2"
                      "[1, 2]"
                      (c/make-Application (c/make-Application (c/make-Variable ":") (c/make-Integer "1"))
                                          (c/make-Application (c/make-Application (c/make-Variable ":") (c/make-Integer "2")) (c/make-ListConstructor))))
                (tcee "ml1"
                      ":ml Int \"x\""
                      (c/make-ML (t/make-Constructor "Int") "x"))
                (tcme "mo1"
                      "{}"
                      (c/make-Module "none" null null))
                (tcme "mo2"
                      "module M where {}"
                      (c/make-Module "M" null null))
                (tcme "mo3"
                      "{ x = 1 }"
                      (c/make-Module "none" null (list (c/make-Declaration "x" (c/make-Integer "1")))))
                (tcme "mo4"
                      "{ x = 1 ; data A = B }"
                      (c/make-Module "none" null (list (c/make-Declaration "x" (c/make-Integer "1"))
                                                       (c/make-Data "A" (list (c/make-Constructor "B" null))))))
                (tcme "mo5"
                      "{ data A = B ; x = 1 }"
                      (c/make-Module "none" null (list (c/make-Data "A" (list (c/make-Constructor "B" null)))
                                                       (c/make-Declaration "x" (c/make-Integer "1")))))
                (tcee "sc1"
                      ":scheme A \"x\""
                      (c/make-Scheme (t/make-Constructor "A") "x"))
                (tcee "tc1"
                      "(,)"
                      (c/make-TupleConstructor 2))
                (tcee "tc2"
                      "(,,)"
                      (c/make-TupleConstructor 3))
                (tcee "tu1"
                      "(1, 2)"
                      (c/make-Application (c/make-Application (c/make-TupleConstructor 2) (c/make-Integer "1")) (c/make-Integer "2")))
                (tcee "tu2"
                      "(1, 2, 3)"
                      (c/make-Application (c/make-Application (c/make-Application (c/make-TupleConstructor 3)
                                                                                  (c/make-Integer "1"))
                                                              (c/make-Integer "2"))
                                          (c/make-Integer "3")))
                (tcte "ty1"
                      "A"
                      (t/make-Constructor "A"))
                (tcte "ty2"
                      "a"
                      (t/make-Variable "a"))
                (tcte "ty3"
                      "A -> B"
                      (t/make-Application (t/make-Application (t/make-Function) (t/make-Constructor "A")) (t/make-Constructor "B")))
                (tcte "ty4"
                      "[A]"
                      (t/make-Application (t/make-List) (t/make-Constructor "A")))
                (tcte "ty5"
                      "(A, B)"
                      (t/make-Application (t/make-Application (t/make-Tuple 2) (t/make-Constructor "A")) (t/make-Constructor "B")))
                (tcte "ty6"
                      "()"
                      (t/make-Unit))
                (tcee "un1"
                      "()"
                      (c/make-UnitConstructor))))
  
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