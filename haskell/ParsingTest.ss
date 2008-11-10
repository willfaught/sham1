(module ParsingTest mzscheme
  (require (lib "contract.ss")
           (planet "main.ss" ("schematics" "schemeunit.plt" 3 3))
           (lib "HaskellSyntax.ss" "sham" "haskell")
           (lib "Maybe.ss" "sham" "haskell")
           (lib "Parsing.ss" "sham" "haskell"))
  
  (provide/contract (testSuite schemeunit-test-suite?))
  
  (define/contract e (-> (-> string? HaskellSyntax?) (-> string? string? HaskellSyntax? schemeunit-test-case?))
    (lambda (parse)
      (lambda (name text syntax)
        (test-equal? name (parse text) syntax))))
  
  (define/contract parseD (-> string? HaskellSyntax?) (declarationParser "test"))
  
  (define/contract parseE (-> string? HaskellSyntax?) (expressionParser "test"))
  
  (define/contract parseI (-> string? HaskellSyntax?) (importParser "test"))
  
  (define/contract parseM (-> string? HaskellSyntax?) (moduleParser "test"))
  
  (define/contract parseT (-> string? HaskellSyntax?) (typeParser "test"))
  
  (define/contract de (-> string? string? HaskellSyntax? schemeunit-test-case?)
    (e parseD))
  
  (define/contract ee (-> string? string? HaskellSyntax? schemeunit-test-case?)
    (e parseE))
  
  (define/contract ie (-> string? string? HaskellSyntax? schemeunit-test-case?)
    (e parseI))
  
  (define/contract me (-> string? string? HaskellSyntax? schemeunit-test-case?)
    (e parseM))
  
  (define/contract te (-> string? string? HaskellSyntax? schemeunit-test-case?)
    (e parseT))
  
  (define/contract testSuite schemeunit-test-suite?
    (test-suite "Parsing"
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
                (ie "im1"
                    "import \"test\" as Test"
                    (make-Import #f "test" "Test" (make-Nothing)))
                (ie "im2"
                    "import qualified \"test\" as Test"
                    (make-Import #t "test" "Test" (make-Nothing)))
                (ie "im3"
                    "import \"test\" as Test ()"
                    (make-Import #f "test" "Test" (make-Nothing)))
                (ie "im4"
                    "import \"test\" as Test hiding ()"
                    (make-Import #f "test" "Test" (make-Nothing)))
                (ie "im5"
                    "import \"test\" as Test (one)"
                    (make-Import #f "test" "Test" (make-Just (list #f (list (list "one" "one"))))))
                (ie "im6"
                    "import \"test\" as Test hiding (one)"
                    (make-Import #f "test" "Test" (make-Just (list #t (list (list "one" "one"))))))
                (ie "im7"
                    "import \"test\" as Test (\"one\" as two)"
                    (make-Import #f "test" "Test" (make-Just (list #f (list (list "one" "two"))))))
                (ie "im8"
                    "import \"test\" as Test (one,)"
                    (make-Import #f "test" "Test" (make-Just (list #f (list (list "one" "one"))))))
                (ie "im9"
                    "import \"test\" as Test (one, two)"
                    (make-Import #f "test" "Test" (make-Just (list #f (list (list "two" "two") (list "one" "one"))))))
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
                    (make-Module "M" null null null))
                (me "mo2"
                    "{}"
                    (make-Module "None" null null null))
                (me "mo3"
                    "{ x = 1 }"
                    (make-Module "None" null null (list (make-Declaration (make-LHS "x" null) (make-Integer "1")))))
                (me "mo4"
                    "{ x = 1 ; data A = B }"
                    (make-Module "None" null null (list (make-Declaration (make-LHS "x" null) (make-Integer "1"))
                                                        (make-Data "A" (list (make-Constructor "B" null))))))
                (me "mo5"
                    "{ data A = B ; x = 1 }"
                    (make-Module "None" null null (list (make-Data "A" (list (make-Constructor "B" null)))
                                                        (make-Declaration (make-LHS "x" null) (make-Integer "1")))))
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
                    (make-UnitConstructor)))))