(module TransformationTest scheme
  (require (planet "main.ss" ("schematics" "schemeunit.plt" 3 3))
           (prefix-in c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix-in h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "Parsing.ss" "sham" "haskell")
           (lib "Transformation.ss" "sham" "haskell")
           (prefix-in t/ (lib "Types.ss" "sham")))
  
  (provide testSuite)
  
  (define (de name declaration syntax)
    (test-equal? name (transformSyntax (parseD declaration)) syntax))
  
  (define (ee name expression syntax)
    (test-equal? name (transformSyntax (parseE expression)) syntax))
  
  (define (ie name import syntax)
    (test-equal? name (transformSyntax (parseI import)) syntax))
  
  (define (me name module syntax)
    (test-equal? name (transformSyntax (parseM module)) syntax))
  
  (define testParsers (parsers "TransformationTest"))
  
  (define parseD (parser 'declaration testParsers))
  
  (define parseE (parser 'expression testParsers))
  
  (define parseI (parser 'import testParsers))
  
  (define parseM (parser 'module testParsers))
  
  (define parseT (parser 'type testParsers))
  
  (define (te name type syntax)
    (test-equal? name (parseT type) syntax))
  
  (define testSuite
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
                    (c/make-Data "A" null (list (c/make-Constructor "B" null))))
                (de "da2"
                    "data A = B {}"
                    (c/make-Data "A" null (list (c/make-Constructor "B" null))))
                (de "da3"
                    "data A = B { c :: D }"
                    (c/make-Data "A" null (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "D")))))))
                (de "da4"
                    "data A = B { c, d :: E }"
                    (c/make-Data "A" null (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "E"))
                                                                         (c/make-Field "d" (t/make-Constructor "E")))))))
                (de "da5"
                    "data A = B { c :: D, e :: F }"
                    (c/make-Data "A" null (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "D"))
                                                                         (c/make-Field "e" (t/make-Constructor "F")))))))
                (de "da6"
                    "data A = B { c, d :: E, f, g :: H }"
                    (c/make-Data "A" null (list (c/make-Constructor "B" (list (c/make-Field "c" (t/make-Constructor "E"))
                                                                         (c/make-Field "d" (t/make-Constructor "E"))
                                                                         (c/make-Field "f" (t/make-Constructor "H"))
                                                                         (c/make-Field "g" (t/make-Constructor "H")))))))
                (de "da7"
                    "data A = B | C"
                    (c/make-Data "A" null (list (c/make-Constructor "B" null) (c/make-Constructor "C" null))))
                (de "da8"
                    "data A b = C"
                    (c/make-Data "A" (list "b") (list (c/make-Constructor "C" null))))
                (de "da9"
                    "data A b c = D"
                    (c/make-Data "A" (list "b" "c") (list (c/make-Constructor "D" null))))
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
                (ie "im1"
                    "import haskell Test (one :: A)"
                    (list (c/make-Import "haskell" "Test" "one" (t/make-Constructor "A"))))
                (ie "im2"
                    "import haskell Test1.Test2 (one :: A)"
                    (list (c/make-Import "haskell" "Test1.Test2" "one" (t/make-Constructor "A"))))
                (ie "im3"
                    "import haskell Test (one :: A, two :: B)"
                    (list (c/make-Import "haskell" "Test" "one" (t/make-Constructor "A"))
                          (c/make-Import "haskell" "Test" "two" (t/make-Constructor "B"))))
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
                    (c/make-Application (c/make-Application (c/make-Variable "Haskell.:") (c/make-Integer "1")) (c/make-ListConstructor)))
                (ee "li2"
                    "[1, 2]"
                    (c/make-Application (c/make-Application (c/make-Variable "Haskell.:") (c/make-Integer "1"))
                                        (c/make-Application (c/make-Application (c/make-Variable "Haskell.:") (c/make-Integer "2")) (c/make-ListConstructor))))
                #|(me "mo1"
                    "{}"
                    (c/make-Module "None" null null null))
                (me "mo2"
                    "module M where {}"
                    (c/make-Module "M" null null null))
                (me "mo3"
                    "module M (a) where {}"
                    (c/make-Module "M" (list (c/make-Export "a")) null null))
                (me "mo4"
                    "module M (a, B, (:), (++), (:+)) where {}"
                    (c/make-Module "M" (list (c/make-Export ":+")
                                             (c/make-Export "++")
                                             (c/make-Export ":")
                                             (c/make-Export "B")
                                             (c/make-Export "a")) null null))
                (me "mo5"
                    "module M where { import ml \"file\" (\"a\" as b :: C) }"
                    (c/make-Module "M" null (list (c/make-Import "ml" (list "file") "a" "b" (t/make-Constructor "C"))) null))
                (me "mo6"
                    "module M where { import ml \"file\" (\"a\" as b :: C, \"d\" as e :: F) }"
                    (c/make-Module "M" null (list (c/make-Import "ml" (list "file") "d" "e" (t/make-Constructor "F"))
                                                  (c/make-Import "ml" (list "file") "a" "b" (t/make-Constructor "C"))) null))
                (me "mo7"
                    "module M where { import ml \"file\" (\"a\" as b :: C) ; import scheme \"file\" (\"d\" as e :: F) }"
                    (c/make-Module "M" null (list (c/make-Import "ml" (list "file") "a" "b" (t/make-Constructor "C"))
                                                  (c/make-Import "scheme" (list "file") "d" "e" (t/make-Constructor "F"))) null))
                (me "mo8"
                    "{ x = 1 }"
                    (c/make-Module "None" null null (list (c/make-Declaration "x" (c/make-Integer "1")))))
                (me "mo9"
                    "{ x = 1 ; data A = B }"
                    (c/make-Module "None" null null (list (c/make-Declaration "x" (c/make-Integer "1"))
                                                          (c/make-Data "A" (list (c/make-Constructor "B" null))))))
                (me "mo10"
                    "{ data A = B ; x = 1 }"
                    (c/make-Module "None" null null (list (c/make-Data "A" (list (c/make-Constructor "B" null)))
                                                          (c/make-Declaration "x" (c/make-Integer "1")))))|#
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
