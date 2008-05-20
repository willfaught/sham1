(module compiler-test mzscheme
  (require (only (lib "1.ss" "srfi") circular-list? proper-list? zip)
           (lib "compiler.ss" "haskell")
           (lib "contract.ss")
           (lib "list.ss")
           (lib "match.ss")
           (lib "primitives.ss" "haskell")
           (lib "parsers.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "test.ocaml" "haskell" "lib")
           (lib "types.ss" "haskell")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide run-tests)
  
  (define-struct lump (contents))
  
  ; test-case-dp :: string (string) datum -> schemeunit-test-case
  (define (test-case-dp n d p)
    (test-true n (eval `(begin ,@(foldl append null (map (lambda (x) (compile-data-term (parse-declaration x))) d)) (,p)))))
  
  ; test-case-e :: string string 'a -> schemeunit-test-case
  (define (test-case-e name expression value)
    (test-check name strict-equal? (eval-r expression) value))
  
  ; test-case-fx :: string string ('a -> 'a) -> schemeunit-test-case
  (define (test-case-fx name expression f)
    (test-exn name (lambda (x) #t) (lambda () (f (eval-r expression)))))
  
  ; test-case-he :: string string string 'a -> schemeunit-test-case
  (define (test-case-he name type expression value)
    (test-check name strict-equal? (eval-h type expression) value))
  
  ; test-case-hfx :: string string string ('a -> 'a) -> schemeunit-test-case
  (define (test-case-hfx name type expression f)
    (test-exn name (lambda (x) #t) (lambda () (f (eval-h type expression)))))
  
  ; test-case-hp :: string string string ('a -> boolean) -> schemeunit-test-case
  (define (test-case-hp name type expression predicate)
    (test-pred name predicate (eval-h type expression)))
  
  ; test-case-p :: string string ('a -> boolean) -> schemeunit-test-case
  (define (test-case-p name expression predicate)
    (test-pred name predicate (eval-r expression)))
  
  ; test-case-x :: string string -> schemeunit-test-case
  (define (test-case-x name expression)
    (test-exn name (lambda (x) #t) (lambda () (eval-r expression))))
  
  ; compiler-test-suite :: schemeunit-test-suite
  (define compiler-test-suite
    (test-suite "compiler"
                (test-case-e "ap1" "(\\x -> x) 1" 1)
                (test-case-e "ap2" "(\\x y -> x) 1 2" 1)
                (test-case-x "ap3" "1 2")
                (test-case-x "ap4" "(\\x -> x) 1 2")
                (test-case-e "ch1" "'a'" #\a)
                (test-case-dp "da1"
                              (list "data A = A")
                              `(lambda () (equal? (force haskell:A) (make-haskell-constructor:A))))
                (test-case-dp "da2"
                              (list "data A = A")
                              `(lambda () (equal? ((force haskell:isA) (delay (force haskell:A))) (force haskell:True))))
                (test-case-dp "da3"
                              (list "data A = A")
                              `(lambda () (haskell-type:A? (force haskell:A))))
                (test-case-dp "da4"
                              (list "data A = B | C")
                              `(lambda () (equal? ((force haskell:isB) (delay (force haskell:B))) (force haskell:True))))
                (test-case-dp "da5"
                              (list "data A = B | C")
                              `(lambda () (equal? ((force haskell:isB) (delay (force haskell:C))) (force haskell:False))))
                (test-case-dp "da6"
                              (list "data A = B | C")
                              `(lambda () (equal? ((force haskell:isC) (delay (force haskell:B))) (force haskell:False))))
                (test-case-dp "da7"
                              (list "data A = B | C")
                              `(lambda () (equal? ((force haskell:isC) (delay (force haskell:C))) (force haskell:True))))
                (test-case-dp "da8"
                              (list "data A = B { c :: A } | D")
                              `(lambda () (equal? ((force haskell:c) (delay ((force haskell:B) (delay (force haskell:D))))) (force haskell:D))))
                (test-case-e "fl1" "1.2" 1.2)
                (test-case-p "fu1" "\\x -> x" (lambda (x) (equal? (x (delay 1)) 1)))
                (test-case-fx "fu2" "\\x -> x" (lambda (x) ((x (delay 1)) (delay 2))))
                (test-case-p "fu3" "\\x y -> x" (lambda (x) (equal? ((x (delay 1)) (delay 2)) 1)))
                (test-case-fx "fu4" "\\x y -> x" (lambda (x) (((x (delay 1)) (delay 2)) (delay 3))))
                (test-case-he "ha1" "Char" "'a'" #\a)
                (test-case-he "ha2" "Float" "1.2" 1.2)
                (test-case-hp "ha3" "[Int] -> [Int]" "\\x -> x" (lambda (x) (strict-equal? (x (list 1)) (cons (delay 1) (delay null)))))
                (test-case-hfx "ha4" "[Int] -> [Int]" "\\x -> x" (lambda (x) (x 1)))
                (test-case-he "ha5" "Int" "1" 1)
                (test-case-he "ha6" "[a]" "[1]" (cons (delay 1) (delay null)))
                (test-case-he "ha7" "(Int, Int)" "(1, 2)" (vector-immutable (delay 1) (delay 2)))
                (test-case-hp "ha8" "a" "1" (lambda (x) (equal? (lump-contents x) 1)))
                (test-case-x "id1" "x")
                (test-case-p "id2" "fst" (lambda (x) (equal? (x (delay (vector-immutable (delay 1) (delay 2)))) 1)))
                (test-case-p "id3" "head" (lambda (x) (equal? (x (delay (cons (delay 1) (delay null)))) 1)))
                (test-case-p "id4" "isFalse" (lambda (x) (strict-equal? (x (delay (force haskell:False))) (force haskell:True))))
                (test-case-p "id5" "isFalse" (lambda (x) (strict-equal? (x (delay (force haskell:True))) (force haskell:False))))
                (test-case-p "id6" "isTrue" (lambda (x) (strict-equal? (x (delay (force haskell:False))) (force haskell:False))))
                (test-case-p "id7" "isTrue" (lambda (x) (strict-equal? (x (delay (force haskell:True))) (force haskell:True))))
                (test-case-p "id8" "null" (lambda (x) (equal? (x (delay null)) (force haskell:True))))
                (test-case-p "id9" "null" (lambda (x) (equal? (x (delay (cons (delay 1) (delay null)))) (force haskell:False))))
                (test-case-p "id10" "snd" (lambda (x) (equal? (x (delay (vector-immutable (delay 1) (delay 2)))) 2)))
                (test-case-p "id11" "tail" (lambda (x) (strict-equal? (x (delay (cons (delay 1) (delay null)))) null)))
                (test-case-p "id12" "False" (lambda (x) (equal? x (force haskell:False))))
                (test-case-p "id13" "True" (lambda (x) (equal? x (force haskell:True))))
                (test-case-p "id14" "(:)" (lambda (x) (strict-equal? ((x (delay 1)) (delay null)) (cons (delay 1) (delay null)))))
                (test-case-p "id15" "()" (lambda (x) (equal? x (force haskell:|()|))))
                (test-case-e "if1" "if True then 1 else 2" 1)
                (test-case-e "if2" "if False then 1 else 2" 2)
                (test-case-e "in1" "1" 1)
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
                (test-case-e "li1" "[]" null)
                (test-case-e "li2" "[1]" (cons (delay 1) (delay null)))
                (test-case-e "li3" "[1, 2]" (cons (delay 1) (delay (cons (delay 2) (delay null)))))
                (test-case-e "ml-bo1" ":ml Bool \"mlBoolean1\"" (force haskell:False))
                (test-case-x "ml-bo2" ":ml Bool \"mlFunction1\"")
                (test-case-x "ml-bo3" ":ml Bool \"mlInteger\"")
                (test-case-x "ml-bo4" ":ml Bool \"mlString\"")
                (test-case-x "ml-bo5" ":ml Bool \"mlTuple\"")
                (test-case-x "ml-bo6" ":ml Bool \"mlUnit\"")
                (test-case-x "ml-in1" ":ml Int \"mlBoolean1\"")
                (test-case-x "ml-in2" ":ml Int \"mlFunction1\"")
                (test-case-e "ml-in3" ":ml Int \"mlInteger\"" 1)
                (test-case-x "ml-in4" ":ml Int \"mlString\"")
                (test-case-x "ml-in5" ":ml Int \"mlTuple\"")
                (test-case-x "ml-in6" ":ml Int \"mlUnit\"")
                (test-case-x "ml-st1" ":ml [Char] \"mlBoolean1\"")
                (test-case-x "ml-st2" ":ml [Char] \"mlFunction1\"")
                (test-case-x "ml-st3" ":ml [Char] \"mlInteger\"")
                (test-case-e "ml-st4" ":ml [Char] \"mlString\"" (cons (delay #\a) (delay (cons (delay #\b) (delay null)))))
                (test-case-x "ml-st5" ":ml [Char] \"mlTuple\"")
                (test-case-x "ml-st6" ":ml [Char] \"mlUnit\"")
                (test-case-x "ml-fu1" ":ml Int -> Int \"mlBoolean1\"")
                (test-case-p "ml-fu2" ":ml Int -> Int \"mlFunction1\"" (lambda (x) (equal? (x (delay 1)) 1)))
                (test-case-x "ml-fu3" ":ml Int -> Int \"mlInteger\"")
                (test-case-x "ml-fu4" ":ml Int -> Int \"mlString\"")
                (test-case-x "ml-fu5" ":ml Int -> Int \"mlTuple\"")
                (test-case-x "ml-fu6" ":ml Int -> Int \"mlUnit\"")
                (test-case-p "ml-fu7"
                             ":ml Bool -> Bool \"mlFunction1\""
                             (lambda (x)
                               (strict-equal? (x (delay (force haskell:False)))
                                              (force haskell:False))))
                (test-case-p "ml-fu8"
                             ":ml ((Bool, Bool), (Bool, Bool)) -> ((Bool, Bool), (Bool, Bool)) \"mlFunction1\""
                             (lambda (x)
                               (let ((v (vector-immutable (delay (vector-immutable (delay (force haskell:True))
                                                                                   (delay (force haskell:False))))
                                                          (delay (vector-immutable (delay (force haskell:True))
                                                                                   (delay (force haskell:False)))))))
                                 (strict-equal? (x (delay v)) v))))
                (test-case-p "ml-fu9" ":ml () -> () \"mlFunction1\"" (lambda (x) (equal? (x (delay (force haskell:|()|))) (force haskell:|()|))))
                (test-case-p "ml-fu10"
                             ":ml a -> a \"mlFunction1\""
                             (lambda (x)
                               (let ((s (cons (delay #\a)
                                              (delay (cons (delay #\b)
                                                           (delay null))))))
                                 (strict-equal? (x (delay s)) s))))
                (test-case-x "ml-tu1" ":ml (Bool, [Char]) \"mlBoolean1\"")
                (test-case-x "ml-tu2" ":ml (Bool, [Char]) \"mlFunction1\"")
                (test-case-x "ml-tu3" ":ml (Bool, [Char]) \"mlInteger\"")
                (test-case-x "ml-tu4" ":ml (Bool, [Char]) \"mlString\"")
                (test-case-e "ml-tu5"
                             ":ml (Bool, [Char]) \"mlTuple\""
                             (vector-immutable (delay (force haskell:False))
                                               (delay (cons (delay #\a)
                                                            (delay (cons (delay #\b)
                                                                         (delay null)))))))
                (test-case-x "ml-tu6" ":ml (Bool, [Char]) \"mlUnit\"")
                (test-case-x "ml-un1" ":ml () \"mlBoolean1\"")
                (test-case-x "ml-un2" ":ml () \"mlFunction1\"")
                (test-case-x "ml-un3" ":ml () \"mlInteger\"")
                (test-case-x "ml-un4" ":ml () \"mlString\"")
                (test-case-x "ml-un5" ":ml () \"mlTuple\"")
                (test-case-e "ml-un6" ":ml () \"mlUnit\"" (force haskell:|()|))
                (test-case-e "sc-ch1" ":scheme Char \"scheme-character\"" #\a)
                (test-case-x "sc-ch2" ":scheme Char \"scheme-float\"")
                (test-case-x "sc-ch3" ":scheme Char \"scheme-function\"")
                (test-case-x "sc-ch4" ":scheme Char \"scheme-integer\"")
                (test-case-x "sc-ch5" ":scheme Char \"scheme-list\"")
                (test-case-x "sc-ch6" ":scheme Char \"scheme-tuple\"")
                (test-case-x "sc-fl1" ":scheme Float \"scheme-character\"")
                (test-case-e "sc-fl2" ":scheme Float \"scheme-float\"" 1.2)
                (test-case-x "sc-fl3" ":scheme Float \"scheme-function\"")
                (test-case-e "sc-fl4" ":scheme Float \"scheme-integer\"" 1)
                (test-case-x "sc-fl5" ":scheme Float \"scheme-list\"")
                (test-case-x "sc-fl6" ":scheme Float \"scheme-tuple\"")
                (test-case-fx "sc-fu1" ":scheme [Int] -> [Int] \"scheme-character\"" (lambda (x) (x (delay 1))))
                (test-case-fx "sc-fu2" ":scheme [Int] -> [Int] \"scheme-float\"" (lambda (x) (x (delay 1))))
                (test-case-p "sc-fu3" ":scheme [Int] -> [Int] \"scheme-function\"" (lambda (x) (strict-equal? (x (delay null)) (cons (delay 1) (delay null)))))
                (test-case-fx "sc-fu4" ":scheme [Int] -> [Int] \"scheme-integer\"" (lambda (x) (x (delay 1))))
                (test-case-fx "sc-fu5" ":scheme [Int] -> [Int] \"scheme-list\"" (lambda (x) (x (delay 1))))
                (test-case-fx "sc-fu6" ":scheme [Int] -> [Int] \"scheme-tuple\"" (lambda (x) (x (delay 1))))
                (test-case-p "sc-fu7" ":scheme Int -> Int -> Int \"primitive:number-add\"" (lambda (x) (equal? ((x (delay 1)) (delay 2)) 3)))
                (test-case-x "sc-in1" ":scheme Int \"scheme-character\"")
                (test-case-x "sc-in2" ":scheme Int \"scheme-float\"")
                (test-case-x "sc-in3" ":scheme Int \"scheme-function\"")
                (test-case-e "sc-in4" ":scheme Int \"scheme-integer\"" 1)
                (test-case-x "sc-in5" ":scheme Int \"scheme-list\"")
                (test-case-x "sc-in6" ":scheme Int \"scheme-tuple\"")
                (test-case-x "sc-li1" ":scheme [a] \"scheme-character\"")
                (test-case-x "sc-li2" ":scheme [a] \"scheme-float\"")
                (test-case-x "sc-li3" ":scheme [a] \"scheme-function\"")
                (test-case-x "sc-li4" ":scheme [a] \"scheme-integer\"")
                (test-case-e "sc-li5" ":scheme [a] \"scheme-list\"" (cons (delay 1) (delay null)))
                (test-case-x "sc-li6" ":scheme [a] \"scheme-tuple\"")
                (test-case-x "sc-tu1" ":scheme (Int, Int) \"scheme-character\"")
                (test-case-x "sc-tu2" ":scheme (Int, Int) \"scheme-float\"")
                (test-case-x "sc-tu3" ":scheme (Int, Int) \"scheme-function\"")
                (test-case-x "sc-tu4" ":scheme (Int, Int) \"scheme-integer\"")
                (test-case-x "sc-tu5" ":scheme (Int, Int) \"scheme-list\"")
                (test-case-e "sc-tu6" ":scheme (Int, Int) \"scheme-tuple\"" (vector-immutable (delay 1) (delay 2)))
                (test-case-p "tu1" "(1, 2)" (lambda (x) (immutable? x)))
                (test-case-e "tu2" "(1, 2)" (vector-immutable (delay 1) (delay 2)))
                (test-case-p "tu3" "(1, 2, 3)" (lambda (x) (immutable? x)))
                (test-case-e "tu4" "(1, 2, 3)" (vector-immutable (delay 1) (delay 2) (delay 3)))
                (test-case-p "tc1" "(,)" (lambda (x) (immutable? ((x (delay 1)) (delay 2)))))
                (test-case-p "tc2" "(,)" (lambda (x) (strict-equal? ((x (delay 1)) (delay 2)) (vector-immutable (delay 1) (delay 2)))))
                (test-case-p "tc3" "(,,)" (lambda (x) (immutable? (((x (delay 1)) (delay 2)) (delay 3)))))
                (test-case-p "tc4" "(,,)" (lambda (x) (strict-equal? (((x (delay 1)) (delay 2)) (delay 3)) (vector-immutable (delay 1) (delay 2) (delay 3)))))))
  
  ; eval-h :: string string -> 'a
  (define (eval-h type expression)
    (eval (compile-term (make-haskell-term (parse-type type) (parse-expression expression)))))
  
  ; eval-r :: string -> 'a
  (define (eval-r expression)
    (eval (compile-term (parse-expression expression))))
  
  ; parse-declaration :: string -> term
  (define (parse-declaration d)
    (let ((port (open-input-string d)))
      (port-count-lines! port)
      (test-declaration-parser (lambda () (language-lexer port)))))
  
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
  
  ; run-tests :: (string)
  (define (run-tests)
    (define (results x y)
      (cond ((test-failure? x) (cons (test-result-test-case-name x) y))
            ((test-error? x) (cons (test-result-test-case-name x) y))
            (else y)))
    (fold-test-results results null compiler-test-suite))
  
  ; scheme-character :: char
  (define scheme-character #\a)
  
  ; scheme-float :: number
  (define scheme-float 1.2)
  
  ; scheme-function :: 'a -> (integer)
  (define scheme-function (lambda (x) (list 1)))
  
  ; scheme-integer :: integer
  (define scheme-integer 1)
  
  ; scheme-list :: (integer)
  (define scheme-list (list 1))
  
  ; scheme-tuple :: #(integer integer)
  (define scheme-tuple (vector 1 2))
  
  ; strict-equal? :: 'a 'a -> boolean
  (define (strict-equal? x y)
    (cond ((and (promise? x) (promise? y)) (strict-equal? (force x) (force y)))
          ((and (pair? x) (pair? y)) (and (strict-equal? (car x) (car y)) (strict-equal? (cdr x) (cdr y))))
          ((and (vector? x) (vector? y) (equal? (vector-length x) (vector-length y)))
           (foldl (lambda (x y) (and y (strict-equal? (list-ref x 0) (list-ref x 1)))) #t (zip (vector->list x) (vector->list y))))
          (else (equal? x y))))
  
  ; test-declaration-parser :: parser
  (define test-declaration-parser (declaration-parser "test"))
  
  ; test-expression-parser :: parser
  (define test-expression-parser (expression-parser "test"))
  
  ; test-type-parser :: parser
  (define test-type-parser (type-parser "test")))