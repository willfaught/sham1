(module ParsingTest scheme
  (require (planet schematics/schemeunit:3:3)
           (prefix-in h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "Maybe.ss" "sham" "haskell")
           (lib "Parsing.ss" "sham" "haskell")
           (prefix-in t/ (lib "Types.ss" "sham")))
  
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
                    (h/make-Data "A" null (list (h/make-Constructor "B" null))))
                (de "da2"
                    "data A = B {}"
                    (h/make-Data "A" null (list (h/make-Constructor "B" null))))
                (de "da3"
                    "data A = B | C"
                    (h/make-Data "A" null (list (h/make-Constructor "B" null)
                                           (h/make-Constructor "C" null))))
                (de "da4"
                    "data A = B { c :: A }"
                    (h/make-Data "A" null (list (h/make-Constructor "B" (list (h/make-Field "c" (t/make-Constructor "A")))))))
                (de "da5"
                    "data A = B { c :: A, d :: A }"
                    (h/make-Data "A" null (list (h/make-Constructor "B" (list (h/make-Field "c" (t/make-Constructor "A"))
                                                                         (h/make-Field "d" (t/make-Constructor "A")))))))
                (de "da6"
                    "data A = B { c, d :: A }"
                    (h/make-Data "A" null (list (h/make-Constructor "B" (list (h/make-Field "c" (t/make-Constructor "A"))
                                                                         (h/make-Field "d" (t/make-Constructor "A")))))))
                (de "da7"
                    "data A b = C"
                    (h/make-Data "A" (list "b") (list (h/make-Constructor "C" null))))
                (de "da8"
                    "data A b c = D"
                    (h/make-Data "A" (list "b" "c") (list (h/make-Constructor "D" null))))
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
                    "import haskell Test (one :: A)"
                    (h/make-Impdecl "haskell" "Test" (list (h/make-Import "one" (t/make-Constructor "A")))))
                (ie "im2"
                    "import ml Test (one :: A)"
                    (h/make-Impdecl "ml" "Test" (list (h/make-Import "one" (t/make-Constructor "A")))))
                (ie "im3"
                    "import scheme Test (one :: A)"
                    (h/make-Impdecl "scheme" "Test" (list (h/make-Import "one" (t/make-Constructor "A")))))
                (ie "im4"
                    "import ml Test1.Test2 (one :: A)"
                    (h/make-Impdecl "ml" "Test1.Test2" (list (h/make-Import "one" (t/make-Constructor "A")))))
                (ie "im5"
                    "import haskell Test (one :: A, two :: B)"
                    (h/make-Impdecl "haskell" "Test" (list (h/make-Import "one" (t/make-Constructor "A"))
                                                           (h/make-Import "two" (t/make-Constructor "B")))))
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
                (me "mo1"
                    "module M where {}"
                    (h/make-Module "M" (make-Nothing) null null))
                (me "mo2"
                    "module M () where {}"
                    (h/make-Module "M" (make-Just null) null null))
                (me "mo3"
                    "module M (a) where {}"
                    (h/make-Module "M" (make-Just (list "a")) null null))
                (me "mo4"
                    "module M (a, (+), A, (:+), (:)) where {}"
                    (h/make-Module "M" (make-Just (list "a" "+" "A" ":+" ":")) null null))
                (me "mo5"
                    "module M where { import haskell Test (one :: A) }"
                    (h/make-Module "M" (make-Nothing) (list (h/make-Impdecl "haskell" "Test" (list (h/make-Import "one" (t/make-Constructor "A"))))) null))
                (me "mo6"
                    "module M where { data A = B }"
                    (h/make-Module "M" (make-Nothing) null (list (h/make-Data "A" null (list (h/make-Constructor "B" null))))))
                (me "mo7"
                    "module M where { x = 1 }"
                    (h/make-Module "M" (make-Nothing) null (list (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1")))))
                (me "mo8"
                    "module M where { x = 1 ; data A = B }"
                    (h/make-Module "M" (make-Nothing) null (list (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1"))
                                                          (h/make-Data "A" null (list (h/make-Constructor "B" null))))))
                (me "mo9"
                    "module M where { data A = B ; x = 1 }"
                    (h/make-Module "M" (make-Nothing) null (list (h/make-Data "A" null (list (h/make-Constructor "B" null)))
                                                          (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1")))))
                (me "mo10"
                    "module M where { import haskell Test (one :: A) ; data A = B ; x = 1 }"
                    (h/make-Module "M"
                                   (make-Nothing)
                                   (list (h/make-Impdecl "haskell" "Test" (list (h/make-Import "one" (t/make-Constructor "A")))))
                                   (list (h/make-Data "A" null (list (h/make-Constructor "B" null)))
                                         (h/make-Declaration (h/make-LHS "x" null) (h/make-Integer "1")))))
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
                    (t/make-Constructor "A"))
                (te "ty2"
                    "A -> B"
                    (t/make-Application (t/make-Application (t/make-Function) (t/make-Constructor "A")) (t/make-Constructor "B")))
                (te "ty3"
                    "[A]"
                    (t/make-Application (t/make-List) (t/make-Constructor "A")))
                (te "ty4"
                    "(A, B)"
                    (t/make-Application (t/make-Application (t/make-Tuple 2) (t/make-Constructor "A")) (t/make-Constructor "B")))
                (te "ty5"
                    "a"
                    (t/make-Variable "a"))
                (te "ty6"
                    "()"
                    (t/make-Unit))
                (ee "un1"
                    "()"
                    (h/make-UnitConstructor)))))