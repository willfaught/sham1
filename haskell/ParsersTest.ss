(module ParsersTest mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (only (lib "Parsers.ss" "sham" "haskell") declarationParser expressionParser moduleParser typeParser)
           (lib "HaskellSyntax.ss" "sham" "haskell"))
  
  (provide testSuite)
  
  ; de :: string string HaskellSyntax -> schemeunit-test-case
  (define (de name declaration syntax)
    (test-equal? name (parseD declaration) syntax))
  
  ; ee :: string string HaskellSyntax -> schemeunit-test-case
  (define (ee name expression syntax)
    (test-equal? name (parseE expression) syntax))
  
  ; me :: string string HaskellSyntax -> schemeunit-test-case
  (define (me name module syntax)
    (test-equal? name (parseM module) syntax))
  
  ; te :: string string HaskellSyntax -> schemeunit-test-case
  (define (te name type syntax)
    (test-equal? name (parseT type) syntax))
  
  ; testSuite :: schemeunit-test-suite
  (define testSuite
    (test-suite "Parsers"
                (ee "ap1"
                    "x y"
                    (make-Application (make-Variable "x") (make-Variable "y")))
                (ee "ap2"
                    "x y z"
                    (make-Application (make-Application (make-Variable "x")
                                                        (make-Variable "y"))
                                      (make-Variable "z")))
                (ee "ch1"
                    "'a'"
                    (make-Character "a"))
                (de "da1"
                    "data A = B"
                    (make-Data "A" (list (make-Constructor "B" null))))
                (de "da2"
                    "data A = B {}"
                    (make-Data "A" (list (make-Constructor "B" null))))
                (de "da3"
                    "data A = B | C"
                    (make-Data "A" (list (make-Constructor "B" null)
                                         (make-Constructor "C" null))))
                (de "da4"
                    "data A = B { c :: A }"
                    (make-Data "A" (list (make-Constructor "B" (list (make-Field "c" (make-TypeConstructor "A")))))))
                (de "da5"
                    "data A = B { c :: A, d :: A }"
                    (make-Data "A" (list (make-Constructor "B" (list (make-Field "c" (make-TypeConstructor "A"))
                                                                     (make-Field "d" (make-TypeConstructor "A")))))))
                (de "da6"
                    "data A = B { c, d :: A }"
                    (make-Data "A" (list (make-Constructor "B" (list (make-Field "c" (make-TypeConstructor "A"))
                                                                     (make-Field "d" (make-TypeConstructor "A")))))))
                (de "de1"
                    "x = 1"
                    (make-Declaration (make-LHS "x" null) (make-Integer "1")))
                (de "de2"
                    "x y = 1"
                    (make-Declaration (make-LHS "x" (list "y")) (make-Integer "1")))
                (ee "fl1"
                    "1.2"
                    (make-Float "1.2"))
                (ee "fu1"
                    "\\x -> 1"
                    (make-Function (list "x")
                                   (make-Integer "1")))
                (ee "fu2"
                    "\\x -> \\y -> 1"
                    (make-Function (list "x")
                                   (make-Function (list "y")
                                                  (make-Integer "1"))))
                (ee "fu3"
                    "\\x y -> 1"
                    (make-Function (list "x" "y")
                                   (make-Integer "1")))
                (ee "id1"
                    "x"
                    (make-Variable "x"))
                (ee "id2"
                    "(x)"
                    (make-Variable "x"))
                (ee "id3"
                    "(:)"
                    (make-Variable ":"))
                (ee "if1"
                    "if x then 1 else 2"
                    (make-If (make-Variable "x")
                             (make-Integer "1")
                             (make-Integer "2")))
                (ee "in1"
                    "1"
                    (make-Integer "1"))
                (ee "le1"
                    "let {} in 1"
                    (make-Let null (make-Integer "1")))
                (ee "le2"
                    "let { x = 1 } in x"
                    (make-Let (list (make-Declaration (make-LHS "x" null) (make-Integer "1")))
                              (make-Variable "x")))
                (ee "le3"
                    "let { x = 1 ; y = 1} in x"
                    (make-Let (list (make-Declaration (make-LHS "x" null) (make-Integer "1"))
                                    (make-Declaration (make-LHS "y" null) (make-Integer "1")))
                              (make-Variable "x")))
                (ee "li1"
                    "[]"
                    (make-ListConstructor))
                (ee "li2"
                    "[1]"
                    (make-List (list (make-Integer "1"))))
                (ee "li3"
                    "[1, 2]"
                    (make-List (list (make-Integer "1")
                                     (make-Integer "2"))))
                (ee "ml1"
                    ":ml Int \"x\""
                    (make-ML (make-TypeConstructor "Int")
                             "x"))
                (me "mo1"
                    "module M where {}"
                    (make-Module "M" (make-Body null null)))
                (me "mo2"
                    "{}"
                    (make-Module "none" (make-Body null null)))
                (me "mo3"
                    "{ x = 1 }"
                    (make-Module "none" (make-Body null (list (make-Declaration (make-LHS "x" null) (make-Integer "1"))))))
                (me "mo4"
                    "{ x = 1 ; data A = B }"
                    (make-Module "none" (make-Body null (list (make-Declaration (make-LHS "x" null) (make-Integer "1"))
                                                              (make-Data "A" (list (make-Constructor "B" null)))))))
                (me "mo5"
                    "{ data A = B ; x = 1 }"
                    (make-Module "none" (make-Body null (list (make-Data "A" (list (make-Constructor "B" null)))
                                                              (make-Declaration (make-LHS "x" null) (make-Integer "1"))))))
                (ee "sc1"
                    ":scheme A \"x\""
                    (make-Scheme (make-TypeConstructor "A") "x"))
                (ee "tu1"
                    "(1, 2)"
                    (make-Tuple (list (make-Integer "1") (make-Integer "2"))))
                (ee "tu1"
                    "(1, 2, 3)"
                    (make-Tuple (list (make-Integer "1") (make-Integer "2") (make-Integer "3"))))
                (ee "tc1"
                    "(,)"
                    (make-TupleConstructor 2))
                (ee "tc2"
                    "(,,)"
                    (make-TupleConstructor 3))
                (te "ty1"
                    "A"
                    (make-TypeConstructor "A"))
                (te "ty2"
                    "A -> B"
                    (make-FunctionType (make-TypeConstructor "A") (make-TypeConstructor "B")))
                (te "ty3"
                    "[A]"
                    (make-ListType (make-TypeConstructor "A")))
                (te "ty4"
                    "(A, B)"
                    (make-TupleType (list (make-TypeConstructor "A")
                                          (make-TypeConstructor "B"))))
                (te "ty5"
                    "a"
                    (make-TypeVariable "a"))
                (te "ty6"
                    "()"
                    (make-UnitType))
                (ee "un1"
                    "()"
                    (make-UnitConstructor))))
  
  ; parseD :: string -> HaskellSyntax
  (define parseD (declarationParser "test"))
  
  ; parseE :: string -> HaskellSyntax
  (define parseE (expressionParser "test"))
  
  ; parseM :: string -> HaskellSyntax
  (define parseM (moduleParser "test"))
  
  ; parseT :: string -> HaskellSyntax
  (define parseT (typeParser "test")))