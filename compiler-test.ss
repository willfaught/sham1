(module compiler-test mzscheme
  (require (only (lib "1.ss" "srfi") circular-list? proper-list? zip)
           (lib "compiler.ss" "haskell")
           (lib "contract.ss")
           (lib "list.ss")
           (lib "match.ss")
           (lib "reader.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "types.ss" "haskell")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide run-tests)
  
  ; eval-r :: string -> 'a
  (define (eval-r expression)
    (eval (compile-term (parse-expression expression))))
  
  ; test-case-e :: string string 'a -> schemeunit-test-case
  (define (test-case-e name expression value)
    (test-check name strict-equal? (eval-r expression) value))
  
  ; test-case-p :: string string ('a -> boolean) -> schemeunit-test-case
  (define (test-case-p name expression predicate)
    (test-pred name predicate (eval-r expression)))
  
  ; test-case-x :: string string -> schemeunit-test-case
  (define (test-case-x name expression)
    (test-exn name (lambda (x) #t) (lambda () (eval-r expression))))
  
  ;;;;;;;;;;;;
  
  ; eval-h :: string string -> 'a
  (define (eval-h type expression)
    (let ((t (parse-type type)))
      (eval (compile-term (make-haskell-guard-term t (make-haskell-term t (parse-expression expression)))))))
  
  ; test-case-he :: string string string 'a -> schemeunit-test-case
  (define (test-case-he name type expression value)
    (test-check name strict-equal? (eval-h type expression) value))
  
  ; test-case-hfx :: string string string thunk -> schemeunit-test-case
  (define (test-case-hfx name type expression f)
    (test-exn name (lambda (x) #t) (lambda () (f (eval-h type expression)))))
  
  ; test-case-hp :: string string string ('a -> boolean) -> schemeunit-test-case
  (define (test-case-hp name type expression predicate)
    (test-pred name predicate (eval-h type expression)))
  
  ; test-case-hx :: string string string -> schemeunit-test-case
  (define (test-case-hx name type expression)
    (test-exn name (lambda (x) #t) (lambda () (eval-h type expression))))
  
  ;;;;;;;;;;;;;;
  
  ; compiler-test-suite :: schemeunit-test-suite
  (define compiler-test-suite
    (test-suite "compiler"
                (test-case-e "ap1" "(\\x -> x) 1" 1)
                (test-case-e "ap2" "(\\x y -> x) 1 2" 1)
                (test-case-x "ap3" "1 2")
                (test-case-x "ap4" "(\\x -> x) 1 2")
                (test-case-e "ch1" "'a'" #\a)
                (test-case-e "ch2" "'Z'" #\Z)
                (test-case-e "fl1" "0.0" 0.0)
                (test-case-e "fl2" "1.1" 1.1)
                (test-case-e "fl3" "123456789.987654321" 123456789.987654321)
                (test-case-p "fu1" "\\x -> x" (lambda (x) (equal? (x (delay 1)) 1)))
                (test-case-p "fu2" "\\x y -> x" (lambda (x) (equal? ((x (delay 1)) (delay 2)) 1)))
                (test-case-he "ha-ch1" "Char" "'a'" #\a)
                (test-case-hx "ha-ch2" "Char" "1.2")
                (test-case-hx "ha-ch3" "Char" "\\x -> x")
                (test-case-hx "ha-ch4" "Char" "1")
                (test-case-hx "ha-ch5" "Char" "[]")
                (test-case-hx "ha-ch6" "Char" "(1, 2)")
                (test-case-hx "ha-fl1" "Float" "'a'")
                (test-case-he "ha-fl2" "Float" "1.2" 1.2)
                (test-case-hx "ha-fl3" "Float" "\\x -> x")
                (test-case-he "ha-fl4" "Float" "1" 1)
                (test-case-hx "ha-fl5" "Float" "[]")
                (test-case-hx "ha-fl6" "Float" "(1, 2)")
                (test-case-hfx "ha-fu1" "Int -> Int" "'a'" (lambda (x) (x 1)))
                (test-case-hfx "ha-fu2" "Int -> Int" "1.2" (lambda (x) (x 1)))
                (test-case-hp "ha-fu3" "[Int] -> [Int]" "\\x -> x" (lambda (x) (strict-equal? (x (list 1)) (cons (delay 1) (delay null)))))
                (test-case-hfx "ha-fu4" "[Int] -> [Int]" "\\x -> 1" (lambda (x) (x (list 1))))
                (test-case-hfx "ha-fu5" "[Int] -> [Int]" "\\x -> x" (lambda (x) (x 1)))
                (test-case-hfx "ha-fu6" "Int -> Int" "1" (lambda (x) (x 1)))
                (test-case-hfx "ha-fu7" "Int -> Int" "[]" (lambda (x) (x 1)))
                (test-case-hfx "ha-fu8" "Int -> Int" "(1, 2)" (lambda (x) (x 1)))
                (test-case-hx "ha-in1" "Int" "'a'")
                (test-case-hx "ha-in2" "Int" "1.2")
                (test-case-hx "ha-in3" "Int" "\\x -> x")
                (test-case-he "ha-in4" "Int" "1" 1)
                (test-case-hx "ha-in5" "Int" "[]")
                (test-case-hx "ha-in6" "Int" "(1, 2)")
                (test-case-hx "ha-li1" "[a]" "'a'")
                (test-case-hx "ha-li2" "[a]" "1.2")
                (test-case-hx "ha-li3" "[a]" "\\x -> x")
                (test-case-hx "ha-li4" "[a]" "1")
                (test-case-he "ha-li5" "[a]" "[1]" (cons (delay 1) (delay null)))
                (test-case-hx "ha-li6" "[a]" "(1, 2)")
                (test-case-hx "ha-tu1" "(Int, Int)" "'a'")
                (test-case-hx "ha-tu2" "(Int, Int)" "1.2")
                (test-case-hx "ha-tu3" "(Int, Int)" "\\x -> x")
                (test-case-hx "ha-tu4" "(Int, Int)" "1")
                (test-case-hx "ha-tu5" "(Int, Int)" "[]")
                (test-case-he "ha-tu6" "(Int, Int)" "(1, 2)" (vector-immutable (delay 1) (delay 2)))
                (test-case-x "id1" "x")
                (test-case-e "in1" "0" 0)
                (test-case-e "in2" "1" 1)
                (test-case-e "in3" "123456789" 123456789)
                (test-case-e "le1" "let { i = 1 } in 2" 2)
                (test-case-e "le2" "let { i = 1 ; j = 2 } in 3" 3)
                (test-case-e "le3" "let { i = 1 } in i" 1)
                (test-case-e "le4" "let { i = 1 ; j = i } in j" 1)
                (test-case-e "le5" "let { i = j ; j = 1 } in i" 1)
                (test-case-p "le6" "let { i x = 1 } in i" (lambda (x) (procedure? x)))
                (test-case-p "le7" "let { i x = 1 } in i" (lambda (x) (equal? (x (delay 2)) 1)))
                (test-case-p "le8" "let { i x = x } in i" (lambda (x) (procedure? x)))
                (test-case-p "le9" "let { i x = x } in i" (lambda (x) (equal? (x (delay 1)) 1)))
                (test-case-p "le10" "let { i x y = x } in i" (lambda (x) (procedure? x)))
                (test-case-p "le11" "let { i x y = x } in i" (lambda (x) (procedure? (x (delay 1)))))
                (test-case-p "le12" "let { i x y = x } in i" (lambda (x) (equal? ((x (delay 1)) (delay 2)) 1)))
                #;(test-case-p "le13" "let { i = i } in i" (lambda (x) (equal? x (force x))))
                #;(test-case-p "le14" "let { i = j ; j = i } in (i, j)" (lambda (x) (and (equal? (vector-ref x 0) (force (vector-ref x 1)))
                                                                                          (equal? (force (vector-ref x 0)) (vector-ref x 1)))))
                (test-case-e "li1" "[]" null)
                (test-case-e "li2" "[1]" (cons (delay 1) (delay null)))
                (test-case-e "li3" "[1, 2]" (cons (delay 1) (delay (cons (delay 2) (delay null)))))
                (test-case-p "tu1" "(1, 2)" (lambda (x) (immutable? x)))
                (test-case-e "tu2" "(1, 2)" (vector-immutable (delay 1) (delay 2)))
                (test-case-p "tu3" "(1, 2, 3)" (lambda (x) (immutable? x)))
                (test-case-e "tu4" "(1, 2, 3)" (vector-immutable (delay 1) (delay 2) (delay 3)))
                (test-case-p "tc1" "(,)" (lambda (x) (immutable? ((x (delay 1)) (delay 2)))))
                (test-case-p "tc2" "(,)" (lambda (x) (strict-equal? ((x (delay 1)) (delay 2)) (vector-immutable (delay 1) (delay 2)))))
                (test-case-p "tc3" "(,,)" (lambda (x) (immutable? (((x (delay 1)) (delay 2)) (delay 3)))))
                (test-case-p "tc4" "(,,)" (lambda (x) (strict-equal? (((x (delay 1)) (delay 2)) (delay 3)) (vector-immutable (delay 1) (delay 2) (delay 3)))))))
  
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
  
  ; run-tests :: [string]
  (define (run-tests)
    (define (results x y)
      (cond ((test-failure? x) (cons (test-result-test-case-name x) y))
            ((test-error? x) (cons (test-result-test-case-name x) y))
            (else y)))
    (fold-test-results results null compiler-test-suite))
  
  ; strict-equal? :: 'a 'a -> boolean
  (define (strict-equal? x y)
    (cond ((and (promise? x) (promise? y)) (strict-equal? (force x) (force y)))
          ((and (pair? x) (pair? y)) (and (strict-equal? (car x) (car y)) (strict-equal? (cdr x) (cdr y))))
          ((and (vector? x) (vector? y) (equal? (vector-length x) (vector-length y)))
           (foldl (lambda (x y) (and y (strict-equal? (list-ref x 0) (list-ref x 1)))) #t (zip (vector->list x) (vector->list y))))
          (else (equal? x y))))
  
  ; test-expression-parser :: parser
  (define test-expression-parser (expression-parser "test"))
  
  ; test-type-parser :: parser
  (define test-type-parser (type-parser "test")))