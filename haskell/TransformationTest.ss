(module TransformationTest mzscheme
  (require (lib "contract.ss")
           (planet "main.ss" ("schematics" "schemeunit.plt" 3 3))
           (prefix c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "Parsing.ss" "sham" "haskell")
           (lib "Transformation.ss" "sham" "haskell")
           (prefix t/ (lib "Types.ss" "sham")))
  
  (provide/contract (testSuite schemeunit-test-suite?))
  
  (define/contract de (-> string? string? c/CoreSyntax? schemeunit-test-case?)
    (lambda (name declaration syntax)
      (test-equal? name (transformSyntax (parseD declaration)) syntax)))
  
  (define/contract ee (-> string? string? c/CoreSyntax? schemeunit-test-case?)
    (lambda (name declaration syntax)
      (test-equal? name (transformSyntax (parseE declaration)) syntax)))
  
  (define/contract me (-> string? string? c/CoreSyntax? schemeunit-test-case?)
    (lambda (name declaration syntax)
      (test-equal? name (transformSyntax (parseM declaration)) syntax)))
  
  (define/contract parseD (-> string? h/HaskellSyntax?) (declarationParser "test"))
  
  (define/contract parseE (-> string? h/HaskellSyntax?) (expressionParser "test"))
  
  (define/contract parseM (-> string? h/HaskellSyntax?) (moduleParser "test"))
  
  (define/contract parseT (-> string? h/HaskellSyntax?) (typeParser "test"))
  
  (define/contract te (-> string? string? t/Type? schemeunit-test-case?)
    (lambda (name type syntax)
      (test-equal? name (transformType (parseT type)) syntax)))
  
  (define/contract testSuite schemeunit-test-suite?
    (test-suite "Transformation"
                (ee "ap1"
                    "x y"
                    (c/make-Application (c/make-Variable "x") (c/make-Variable "y")))
                (ee "ap2"
                    "x y z"
                    (c/make-Application (c/make-Application (c/make-Variable "x") (c/make-Variable "y")) (c/make-Variable "z")))
                (ee "ch1"
                    "'a'"
                    (c/make-Character "a"))
                (de "da1"
                    "data A = B"
                    (c/make-Data "A" (list (c/make-Constructor "B" null))))
                (de "da2"
                    "data A = B {}"
                    (c/make-Data "A" (list (c/make-Constructor "B" null))))
                (de "da3"
                    "data A = B { c :: D }"
                    (c/make-Data "A" (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "D")))))))
                (de "da4"
                    "data A = B { c, d :: E }"
                    (c/make-Data "A" (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "E"))
                                                                         (c/make-Field "d" (t/make-Constructor "E")))))))
                (de "da5"
                    "data A = B { c :: D, e :: F }"
                    (c/make-Data "A" (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "D"))
                                                                         (c/make-Field "e" (t/make-Constructor "F")))))))
                (de "da6"
                    "data A = B { c, d :: E, f, g :: H }"
                    (c/make-Data "A" (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "E"))
                                                                         (c/make-Field "d" (t/make-Constructor "E"))
                                                                         (c/make-Field "f" (t/make-Constructor "H"))
                                                                         (c/make-Field "g" (t/make-Constructor "H")))))))
                (de "da7"
                    "data A = B | C"
                    (c/make-Data "A" (list (c/make-Constructor "B" null) (c/make-Constructor "C" null))))
                (de "de1"
                    "x = 1"
                    (c/make-Declaration "x" (c/make-Integer "1")))
                (de "de2"
                    "x y = 1"
                    (c/make-Declaration "x" (c/make-Function "y" (c/make-Integer "1"))))
                (de "de3"
                    "x y z = 1"
                    (c/make-Declaration "x" (c/make-Function "y" (c/make-Function "z" (c/make-Integer "1")))))
                (ee "fl1"
                    "1.2"
                    (c/make-Float "1.2"))
                (ee "fu1"
                    "\\x -> 1"
                    (c/make-Function "x" (c/make-Integer "1")))
                (ee "fu2"
                    "\\x y -> 1"
                    (c/make-Function "x" (c/make-Function "y" (c/make-Integer "1"))))
                (ee "fu3"
                    "\\x -> \\y -> 1"
                    (c/make-Function "x" (c/make-Function "y" (c/make-Integer "1"))))
                (ee "id1"
                    "x"
                    (c/make-Variable "x"))
                (ee "id2"
                    "(x)"
                    (c/make-Variable "x"))
                (ee "id3"
                    "(:)"
                    (c/make-Variable ":"))
                (ee "if1"
                    "if x then y else z"
                    (c/make-If (c/make-Variable "x") (c/make-Variable "y") (c/make-Variable "z")))
                (ee "in1"
                    "1"
                    (c/make-Integer "1"))
                (ee "le1"
                    "let {} in x"
                    (c/make-Let null (c/make-Variable "x")))
                (ee "le2"
                    "let { x = 1 } in x"
                    (c/make-Let (list (c/make-Declaration "x" (c/make-Integer "1"))) (c/make-Variable "x")))
                (ee "le3"
                    "let { x = 1 ; y = 1} in x"
                    (c/make-Let (list (c/make-Declaration "x" (c/make-Integer "1"))
                                      (c/make-Declaration "y" (c/make-Integer "1")))
                                (c/make-Variable "x")))
                (ee "lc1"
                    "[]"
                    (c/make-ListConstructor))
                (ee "li1"
                    "[1]"
                    (c/make-Application (c/make-Application (c/make-Variable ":") (c/make-Integer "1")) (c/make-ListConstructor)))
                (ee "li2"
                    "[1, 2]"
                    (c/make-Application (c/make-Application (c/make-Variable ":") (c/make-Integer "1"))
                                        (c/make-Application (c/make-Application (c/make-Variable ":") (c/make-Integer "2")) (c/make-ListConstructor))))
                (ee "ml1"
                    ":ml Int \"x\""
                    (c/make-ML (h/make-TypeConstructor "Int") "x"))
                (me "mo1"
                    "{}"
                    (c/make-Module "None" null null null))
                (me "mo2"
                    "module M where {}"
                    (c/make-Module "M" null null null))
                (me "mo3"
                    "{ x = 1 }"
                    (c/make-Module "None" null null (list (c/make-Declaration "x" (c/make-Integer "1")))))
                (me "mo4"
                    "{ x = 1 ; data A = B }"
                    (c/make-Module "None" null null (list (c/make-Declaration "x" (c/make-Integer "1"))
                                                          (c/make-Data "A" (list (c/make-Constructor "B" null))))))
                (me "mo5"
                    "{ data A = B ; x = 1 }"
                    (c/make-Module "None" null null (list (c/make-Data "A" (list (c/make-Constructor "B" null)))
                                                          (c/make-Declaration "x" (c/make-Integer "1")))))
                (ee "sc1"
                    ":scheme Int \"x\""
                    (c/make-Scheme (h/make-TypeConstructor "Int") "x"))
                (ee "tc1"
                    "(,)"
                    (c/make-TupleConstructor 2))
                (ee "tc2"
                    "(,,)"
                    (c/make-TupleConstructor 3))
                (ee "tu1"
                    "(1, 2)"
                    (c/make-Application (c/make-Application (c/make-TupleConstructor 2) (c/make-Integer "1")) (c/make-Integer "2")))
                (ee "tu2"
                    "(1, 2, 3)"
                    (c/make-Application (c/make-Application (c/make-Application (c/make-TupleConstructor 3)
                                                                                (c/make-Integer "1"))
                                                            (c/make-Integer "2"))
                                        (c/make-Integer "3")))
                (te "ty1"
                    "A"
                    (t/make-Constructor "A"))
                (te "ty2"
                    "a"
                    (t/make-Variable "a"))
                (te "ty3"
                    "A -> B"
                    (t/make-Application (t/make-Application (t/make-Function) (t/make-Constructor "A")) (t/make-Constructor "B")))
                (te "ty4"
                    "[A]"
                    (t/make-Application (t/make-List) (t/make-Constructor "A")))
                (te "ty5"
                    "(A, B)"
                    (t/make-Application (t/make-Application (t/make-Tuple 2) (t/make-Constructor "A")) (t/make-Constructor "B")))
                (te "ty6"
                    "()"
                    (t/make-Unit))
                (ee "un1"
                    "()"
                    (c/make-UnitConstructor)))))