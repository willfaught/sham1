(module ParsingTest mzscheme
  (require (planet "main.ss" ("schematics" "schemeunit.plt" 3 3))
           (prefix h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "Maybe.ss" "sham" "haskell")
           (lib "Parsing.ss" "sham" "haskell")
           (prefix t/ (lib "Types.ss" "sham")))
  
  (provide testSuite)
  
  (define (e parse)
    (lambda (name text syntax)
      (test-equal? name (parse text) syntax)))
  
  (define testParsers (parsers "ParsingTest"))
  
  (define parseD (parser 'declaration testParsers))
  
  (define parseE (parser 'expression testParsers))
  
  (define parseI (parser 'import testParsers))
  
  (define parseM (parser 'module testParsers))
  
  (define parseT (parser 'type testParsers))
  
  (define de (e parseD))
  
  (define ee (e parseE))
  
  (define ie (e parseI))
  
  (define me (e parseM))
  
  (define te (e parseT))
  
  (define testSuite
    (test-suite "Parsing"
                (ee "ap1"
                    "x y"
                    (h/make-Application (h/make-Variable "x") (h/make-Variable "y")))
                (ee "ap2"
                    "x y z"
                    (h/make-Application (h/make-Application (h/make-Variable "x")
                                                            (h/make-Variable "y"))
                                        (h/make-Variable "z")))
                (ee "ch1"
                    "'a'"
                    (h/make-Character "a"))
                (de "da1"
                    "data A = B"
                    (h/make-Data "A" (list (h/make-Constructor "B" null))))
                (de "da2"
                    "data A = B {}"
                    (h/make-Data "A" (list (h/make-Constructor "B" null))))
                (de "da3"
                    "data A = B | C"
                    (h/make-Data "A" (list (h/make-Constructor "B" null)
                                           (h/make-Constructor "C" null))))
                (de "da4"
                    "data A = B { c :: A }"
                    (h/make-Data "A" (list (h/make-Constructor "B" (list (h/make-Field "c" (h/make-TypeConstructor "A")))))))
                (de "da5"
                    "data A = B { c :: A, d :: A }"
                    (h/make-Data "A" (list (h/make-Constructor "B" (list (h/make-Field "c" (h/make-TypeConstructor "A"))
                                                                         (h/make-Field "d" (h/make-TypeConstructor "A")))))))
                (de "da6"
                    "data A = B { c, d :: A }"
                    (h/make-Data "A" (list (h/make-Constructor "B" (list (h/make-Field "c" (h/make-TypeConstructor "A"))
                                                                         (h/make-Field "d" (h/make-TypeConstructor "A")))))))
                (de "de1"
                    "x = 1"
                    (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1")))
                (de "de2"
                    "x y = 1"
                    (h/make-Declaration (h/make-LHS "x" (list "y")) (h/make-Integer "1")))
                (ee "fl1"
                    "1.2"
                    (h/make-Float "1.2"))
                (ee "fu1"
                    "\\x -> 1"
                    (h/make-Function (list "x")
                                     (h/make-Integer "1")))
                (ee "fu2"
                    "\\x -> \\y -> 1"
                    (h/make-Function (list "x")
                                     (h/make-Function (list "y")
                                                      (h/make-Integer "1"))))
                (ee "fu3"
                    "\\x y -> 1"
                    (h/make-Function (list "x" "y")
                                     (h/make-Integer "1")))
                (ee "id1"
                    "x"
                    (h/make-Variable "x"))
                (ee "id2"
                    "(x)"
                    (h/make-Variable "x"))
                (ee "id3"
                    "(:)"
                    (h/make-Variable ":"))
                (ee "if1"
                    "if x then 1 else 2"
                    (h/make-If (h/make-Variable "x")
                               (h/make-Integer "1")
                               (h/make-Integer "2")))
                (ie "im1"
                    "import ml \"test\" (\"one\" as two :: A)"
                    (h/make-Impdecl "ml" "test" (list (h/make-Import "one" "two" (h/make-TypeConstructor "A")))))
                (ie "im2"
                    "import ml \"test\" (\"one\" as two :: A, \"three\" as four :: B)"
                    (h/make-Impdecl "ml" "test" (list (h/make-Import "three" "four" (h/make-TypeConstructor "B"))
                                                      (h/make-Import "one" "two" (h/make-TypeConstructor "A")))))
                (ie "im3"
                    "import scheme \"test\" (\"one\" as two :: A)"
                    (h/make-Impdecl "scheme" "test" (list (h/make-Import "one" "two" (h/make-TypeConstructor "A")))))
                (ie "im4"
                    "import scheme \"test\" (\"one\" as two :: A, \"three\" as four :: B)"
                    (h/make-Impdecl "scheme" "test" (list (h/make-Import "three" "four" (h/make-TypeConstructor "B"))
                                                          (h/make-Import "one" "two" (h/make-TypeConstructor "A")))))
                (ee "in1"
                    "1"
                    (h/make-Integer "1"))
                (ee "le1"
                    "let {} in 1"
                    (h/make-Let null (h/make-Integer "1")))
                (ee "le2"
                    "let { x = 1 } in x"
                    (h/make-Let (list (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1")))
                                (h/make-Variable "x")))
                (ee "le3"
                    "let { x = 1 ; y = 1} in x"
                    (h/make-Let (list (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1"))
                                      (h/make-Declaration (h/make-LHS "y" null) (h/make-Integer "1")))
                                (h/make-Variable "x")))
                (ee "li1"
                    "[]"
                    (h/make-ListConstructor))
                (ee "li2"
                    "[1]"
                    (h/make-List (list (h/make-Integer "1"))))
                (ee "li3"
                    "[1, 2]"
                    (h/make-List (list (h/make-Integer "1") (h/make-Integer "2"))))
                (ee "ml1"
                    ":ml Int \"x\""
                    (h/make-ML (h/make-TypeConstructor "Int") "x"))
                (me "mo1"
                    "{}"
                    (h/make-Module "None" null null null))
                (me "mo2"
                    "module M where {}"
                    (h/make-Module "M" null null null))
                (me "mo3"
                    "module M (a) where {}"
                    (h/make-Module "M" (list "a") null null))
                (me "mo4"
                    "module M (a, A, (+), (:)) where {}"
                    (h/make-Module "M" (list ":" "+" "A" "a") null null))
                (me "mo5"
                    "{ import ml \"file\" (\"a\" as b :: C) }"
                    (h/make-Module "None" null (list (h/make-Impdecl "ml" "file" (list (h/make-Import "a" "b" (h/make-TypeConstructor "C"))))) null))
                (me "mo6"
                    "{ data A = B }"
                    (h/make-Module "None" null null (list (h/make-Data "A" (list (h/make-Constructor "B" null))))))
                (me "mo7"
                    "{ x = 1 }"
                    (h/make-Module "None" null null (list (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1")))))
                (me "mo8"
                    "{ x = 1 ; data A = B }"
                    (h/make-Module "None" null null (list (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1"))
                                                          (h/make-Data "A" (list (h/make-Constructor "B" null))))))
                (me "mo9"
                    "{ data A = B ; x = 1 }"
                    (h/make-Module "None" null null (list (h/make-Data "A" (list (h/make-Constructor "B" null)))
                                                          (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1")))))
                (me "mo10"
                    "{ import ml \"file\" (\"a\" as b :: C) ; data A = B ; x = 1 }"
                    (h/make-Module "None"
                                   null
                                   (list (h/make-Impdecl "ml" "file" (list (h/make-Import "a" "b" (h/make-TypeConstructor "C")))))
                                   (list (h/make-Data "A" (list (h/make-Constructor "B" null)))
                                         (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1")))))
                (ee "sc1"
                    ":scheme A \"x\""
                    (h/make-Scheme (h/make-TypeConstructor "A") "x"))
                (ee "tu1"
                    "(1, 2)"
                    (h/make-Tuple (list (h/make-Integer "1") (h/make-Integer "2"))))
                (ee "tu1"
                    "(1, 2, 3)"
                    (h/make-Tuple (list (h/make-Integer "1") (h/make-Integer "2") (h/make-Integer "3"))))
                (ee "tc1"
                    "(,)"
                    (h/make-TupleConstructor 2))
                (ee "tc2"
                    "(,,)"
                    (h/make-TupleConstructor 3))
                (te "ty1"
                    "A"
                    (h/make-TypeConstructor "A"))
                (te "ty2"
                    "A -> B"
                    (h/make-FunctionType (h/make-TypeConstructor "A") (h/make-TypeConstructor "B")))
                (te "ty3"
                    "[A]"
                    (h/make-ListType (h/make-TypeConstructor "A")))
                (te "ty4"
                    "(A, B)"
                    (h/make-TupleType (list (h/make-TypeConstructor "A")
                                            (h/make-TypeConstructor "B"))))
                (te "ty5"
                    "a"
                    (h/make-TypeVariable "a"))
                (te "ty6"
                    "()"
                    (h/make-UnitType))
                (ee "un1"
                    "()"
                    (h/make-UnitConstructor)))))