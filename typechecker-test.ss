(module typechecker-test mzscheme
  (require (lib "match.ss")
           (lib "reader.ss" "haskell")
           (lib "typechecker.ss" "haskell")
           (lib "types.ss" "haskell")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide run-tests)
  
  ; run-tests :: [string]
  (define (run-tests)
    (let* ((test-suites (test-suite "all"
                                    application-test-suite
                                    boolean-test-suite
                                    character-test-suite
                                    integer-test-suite
                                    float-test-suite
                                    function-test-suite
                                    identifier-test-suite
                                    if-test-suite
                                    let-test-suite
                                    list-test-suite
                                    tuple-test-suite
                                    tuplecon-test-suite))
           (results (lambda (x y)
                      (cond ((test-failure? x) (cons (test-result-test-case-name x) y))
                            ((test-error? x) (cons (test-result-test-case-name x) y))
                            (else y)))))
      (fold-test-results results null test-suites)))
  
  (define test-expression-parser (expression-parser "test"))
  
  (define test-type-parser (type-parser "test"))
  
  ; parse-expression :: string -> term
  (define (parse-expression expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (test-expression-parser (lambda () (language-lexer port)))))
  
  ; parse-type :: string -> type
  (define (parse-type type)
    (let ((port (open-input-string type)))
      (port-count-lines! port)
      (normalize-type-variables (map-type (lambda (x) (if (type-constructor? x) (translate-type-constructor x) x))
                                          (test-type-parser (lambda () (language-lexer port)))))))
  
  ; test-case-success :: string -> string -> string -> test-case
  (define (test-case-success name expression type)
    (test-equal? name (reconstruct-type (parse-expression expression)) (parse-type type)))
  
  ; test-case-error :: string -> string -> test-case
  (define (test-case-error name expression)
    (test-exn name (lambda (x) #t) (lambda () (reconstruct-type (parse-expression expression)))))
  
  (define application-test-suite
    (test-suite "application"
                (test-case-success "app1" "(\\x -> 1) 1" "Int")
                (test-case-success "app2" "(\\x -> x) 1" "Int")
                (test-case-success "app3" "(\\x -> \\y -> x) 1" "t -> Int")
                (test-case-success "app4" "(\\x y -> x) 1" "t -> Int")
                (test-case-success "app5" "(\\x -> [x]) 1" "[Int]")
                (test-case-success "app6" "(\\x -> (x, x)) 1" "(Int, Int)")
                (test-case-success "app7" "(\\x y -> 1) 2 3" "Int")
                (test-case-error "app8" "1 2")
                (test-case-error "app9" "(\\x -> x) 1 2")
                (test-case-error "app10" "let { i x = x ; j = i 1 } in i 'a'")))
  
  (define boolean-test-suite
    (test-suite "boolean"
                (test-case-success "bool1" "True" "Bool")
                (test-case-success "bool2" "False" "Bool")))
  
  (define character-test-suite
    (test-suite "character"
                (test-case-success "char1" "'a'" "Char")))
  
  (define integer-test-suite
    (test-suite "integer"
                (test-case-success "int1" "1" "Int")))
  
  (define float-test-suite
    (test-suite "float"
                (test-case-success "float1" "1.2" "Float")))
  
  (define function-test-suite
    (test-suite "function"
                (test-case-success "fun1" "\\x -> 1" "t -> Int")
                (test-case-success "fun2" "\\x -> x" "t -> t")
                (test-case-success "fun3" "\\x -> \\y -> x" "t -> t1 -> t")
                (test-case-success "fun4" "\\x y -> x" "t -> t1 -> t")
                (test-case-success "fun5" "\\x -> \\y -> y" "t -> t1 -> t1")
                (test-case-success "fun6" "\\x y -> y" "t -> t1 -> t1")
                (test-case-success "fun7" "\\x -> [x]" "t -> [t]")
                (test-case-success "fun8" "\\x -> (x, x)" "t -> (t, t)")))
  
  (define identifier-test-suite
    (test-suite "identifier"
                (test-case-error "id1" "x")))
  
  (define if-test-suite
    (test-suite "if"
                (test-case-success "if" "if True then 1 else 2" "Int")
                (test-case-success "if" "if False then 1 else 2" "Int")))
  
  (define let-test-suite
    (test-suite "let"
                (test-case-success "let1" "let { i = 'a' } in 1.2" "Float")
                (test-case-success "let2" "let { i = 'a' } in i" "Char")
                (test-case-success "let3" "let { i = i } in i" "t")
                (test-case-success "let4" "let { i = 2 ; j = i } in j" "Int")
                (test-case-success "let5" "let { i = j ; j = 2 } in j" "Int")
                (test-case-success "let6" "let { i x = 2 } in i" "t -> Int")
                (test-case-success "let7" "let { i x = x } in i" "t -> t")
                (test-case-success "let8" "let { i x y = y } in i" "t -> t1 -> t1")
                (test-case-success "let9" "let { i x = x ; j = i 2; k = i 3 } in i" "Int -> Int")
                (test-case-success "let10" "let { i x = 2 } in i 3" "Int")
                (test-case-success "let11" "let { i x = x } in i 2" "Int")
                (test-case-success "let12" "let { i x = x } in let { j = i 2 ; k = i 3.4 } in i" "t -> t")
                (test-case-success "let13" "let { i x = x } in let { j = i 2 ; k = i 3.4 } in j" "Int")))
  
  (define list-test-suite
    (test-suite "list"
                (test-case-success "list1" "[]" "[t]")
                (test-case-success "list2" "[True]" "[Bool]")
                (test-case-success "list3" "['a']" "[Char]")
                (test-case-success "list4" "[1]" "[Int]")
                (test-case-success "list5" "[2.3]" "[Float]")
                (test-case-success "list6" "[\\x -> x]" "[t -> t]")
                (test-case-success "list7" "[[]]" "[[t]]")
                (test-case-success "list8" "[[1]]" "[[Int]]")
                (test-case-success "list9" "[(1, 2)]" "[(Int, Int)]")
                (test-case-success "list10" "[(,)]" "[t -> t1 -> (t, t1)]")
                (test-case-success "list11" "[[], []]" "[[t]]")
                (test-case-error "list12" "['a', 2]")
                (test-case-error "list13" "[[1], ['a']]")))
  
  (define tuple-test-suite
    (test-suite "tuple"
                (test-case-success "tuple1" "(True, False)" "(Bool, Bool)")
                (test-case-success "tuple2" "('a', 'b')" "(Char, Char)")
                (test-case-success "tuple3" "(1, 2)" "(Int, Int)")
                (test-case-success "tuple4" "(1.2, 3.4)" "(Float, Float)")
                (test-case-success "tuple5" "(\\x -> x, \\y -> y)" "(t -> t, t1 -> t1)")
                (test-case-success "tuple6" "([], [])" "([t], [t1])")
                (test-case-success "tuple7" "((1, 2), (3, 4))" "((Int, Int), (Int, Int))")
                (test-case-success "tuple8" "((,), (,))" "(t -> t1 -> (t, t1), t2 -> t3 -> (t2, t3))")
                (test-case-success "tuple" "('a', 1)" "(Char, Int)")
                (test-case-success "tuple" "(1, 2, 3)" "(Int, Int, Int)")))
  
  (define tuplecon-test-suite
    (test-suite "tuplecon"
                (test-case-success "tupcon1" "(,)" "t -> t1 -> (t, t1)")
                (test-case-success "tupcon2" "(,,)" "t -> t1 -> t2 -> (t, t1, t2)")
                (test-case-success "tupcon3" "(,) 1" "t -> (Int, t)")
                (test-case-success "tupcon4" "(,) 1 2" "(Int, Int)")
                (test-case-success "tupcon5" "(,,) 1" "t -> t1 -> (Int, t, t1)")
                (test-case-success "tupcon6" "(,,) 1 2" "t -> (Int, Int, t)")
                (test-case-success "tupcon7" "(,,) 1 2 3" "(Int, Int, Int)")
                (test-case-success "tupcon8" "(,) (,) (,)" "(t -> t1 -> (t, t1), t2 -> t3 -> (t2, t3))")
                (test-case-error "tupcon9" "(,) 1 2 3")
                (test-case-error "tupcon10" "(,,) 1 2 3 4"))))