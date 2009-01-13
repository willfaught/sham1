(module CompilerTest mzscheme
  (require (only (lib "1.ss" "srfi") #;circular-list? #;proper-list? zip)
           (only (lib "list.ss") foldl)
           (planet "main.ss" ("schematics" "schemeunit.plt" 3 3))
           (lib "Compiler.ss" "sham" "haskell")
           #;(lib "ml.ml" "sham" "examples")
           (only (lib "Parsers.ss" "sham" "haskell") declarationParser expressionParser)
           (lib "Primitives.ss" "sham" "haskell")
           (only (lib "SyntaxTransformer.ss" "sham" "haskell") transformHC)
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide testSuite)
  
  ; dp :: string string datum -> schemeunit-test-case
  (define (dp name data predicate)
    (test-true name (eval `(begin ,@(compileCS (transformHC (parseD data))) (,predicate)))))
  
  ; evalE :: string -> datum
  (define (evalE expression)
    (eval (compileCS (transformHC (parseE expression)))))
  
  ; ee :: string string 'a -> schemeunit-test-case
  (define (ee name expression value)
    (test-check name equal (evalE expression) value))
  
  ; efx :: string string ('a -> 'b) -> schemeunit-test-case
  (define (efx name expression f)
    (test-exn name (lambda (x) #t) (lambda () (f (evalE expression)))))
  
  ; ep :: string string ('a -> boolean) -> schemeunit-test-case
  (define (ep name expression predicate)
    (test-pred name predicate (evalE expression)))
  
  ; equal :: 'a 'a -> boolean
  (define (equal x y)
    (cond ((and (promise? x) (promise? y)) (equal (force x) (force y)))
          ((and (haskell/constructor/#Cons? x) (haskell/constructor/#Cons? y))
           (and (equal (force (haskell/constructor/#Cons-head x))
                       (force (haskell/constructor/#Cons-head y)))
                (equal (force (haskell/constructor/#Cons-tail x))
                       (force (haskell/constructor/#Cons-tail y)))))
          ((and (vector? x) (vector? y) (equal? (vector-length x) (vector-length y)))
           (foldl (lambda (x y) (and y (equal (list-ref x 0) (list-ref x 1)))) #t (zip (vector->list x) (vector->list y))))
          (else (equal? x y))))
  
  ; ex :: string string -> schemeunit-test-case
  (define (ex name expression)
    (test-exn name (lambda (x) #t) (lambda () (evalE expression))))
  
  ; parseD :: string -> HaskellSyntax
  (define parseD (declarationParser "test"))
  
  ; parseE :: string -> HaskellSyntax
  (define parseE (expressionParser "test"))
  
  ; testSuite :: schemeunit-test-suite
  (define testSuite
    (test-suite "Compiler"
                (ee "ap1" "(\\x -> x) 1" 1)
                (ee "ap2" "(\\x y -> x) 1 2" 1)
                (ex "ap3" "1 2")
                (ex "ap4" "(\\x -> x) 1 2")
                (ep "ap5" "(\\x y -> x) 1" (lambda (x) (equal? (x (delay 2)) 1)))
                (ee "ch1" "'a'" #\a)
                (dp "da1"
                    "data A = A"
                    '(lambda () (equal? (force haskell/A) (make-haskell/constructor/A))))
                (dp "da2"
                    "data A = A"
                    `(lambda () (equal? ((force haskell/isA) (delay (force haskell/A))) (force haskell/True))))
                (dp "da3"
                    "data A = A"
                    `(lambda () (haskell/type/A? (force haskell/A))))
                (dp "da4"
                    "data A = B | C"
                    `(lambda () (equal? ((force haskell/isB) (delay (force haskell/B))) (force haskell/True))))
                (dp "da5"
                    "data A = B | C"
                    `(lambda () (equal? ((force haskell/isB) (delay (force haskell/C))) (force haskell/False))))
                (dp "da6"
                    "data A = B | C"
                    `(lambda () (equal? ((force haskell/isC) (delay (force haskell/B))) (force haskell/False))))
                (dp "da7"
                    "data A = B | C"
                    `(lambda () (equal? ((force haskell/isC) (delay (force haskell/C))) (force haskell/True))))
                (dp "da8"
                    "data A = B { c :: A } | D"
                    `(lambda () (equal? ((force haskell/c) (delay ((force haskell/B) (delay (force haskell/D))))) (force haskell/D))))
                (ee "fl1" "1.2" 1.2)
                (ep "fu1" "\\x -> x" (lambda (x) (equal? (x (delay 1)) 1)))
                (efx "fu2" "\\x -> x" (lambda (x) ((x (delay 1)) (delay 2))))
                (ep "fu3" "\\x y -> x" (lambda (x) (equal? ((x (delay 1)) (delay 2)) 1)))
                (efx "fu4" "\\x y -> x" (lambda (x) (((x (delay 1)) (delay 2)) (delay 3))))
                (ex "id1" "x")
                (ep "id2" "fst" (lambda (x) (equal? (x (delay (vector-immutable (delay 1) (delay 2)))) 1)))
                (ep "id3" "head" (lambda (x) (equal? (x (delay (make-haskell/constructor/#Cons (delay 1) (delay (make-haskell/constructor/#Nil))))) 1)))
                (ep "id4" "isFalse" (lambda (x) (equal (x (delay (force haskell/False))) (force haskell/True))))
                (ep "id5" "isFalse" (lambda (x) (equal (x (delay (force haskell/True))) (force haskell/False))))
                (ep "id6" "isTrue" (lambda (x) (equal (x (delay (force haskell/False))) (force haskell/False))))
                (ep "id7" "isTrue" (lambda (x) (equal (x (delay (force haskell/True))) (force haskell/True))))
                (ep "id8" "null" (lambda (x) (equal? (x (delay (make-haskell/constructor/#Nil))) (force haskell/True))))
                (ep "id9" "null" (lambda (x) (equal? (x (delay (cons (delay 1) (delay null)))) (force haskell/False))))
                (ep "id10" "snd" (lambda (x) (equal? (x (delay (vector-immutable (delay 1) (delay 2)))) 2)))
                (ep "id11"
                    "tail"
                    (lambda (x) (equal (x (delay (make-haskell/constructor/#Cons (delay 1) (delay (make-haskell/constructor/#Nil))))) (make-haskell/constructor/#Nil))))
                (ep "id12" "False" (lambda (x) (equal? x (force haskell/False))))
                (ep "id13" "True" (lambda (x) (equal? x (force haskell/True))))
                (ep "id14"
                    "(:)"
                    (lambda (x) (equal ((x (delay 1)) (delay (make-haskell/constructor/#Nil)))
                                       (make-haskell/constructor/#Cons (delay 1) (delay (make-haskell/constructor/#Nil))))))
                (ee "if1" "if True then 1 else 2" 1)
                (ee "if2" "if False then 1 else 2" 2)
                (ee "in1" "1" 1)
                (ee "le1" "let { i = 1 } in 2" 2)
                (ee "le2" "let { i = 1 ; j = 2 } in 3" 3)
                (ee "le3" "let { i = 1 } in i" 1)
                (ee "le4" "let { i = 1 ; j = i } in j" 1)
                (ee "le5" "let { i = j ; j = 1 } in i" 1)
                (ep "le6" "let { i x = 1 } in i" (lambda (x) (procedure? x)))
                (ep "le7" "let { i x = 1 } in i" (lambda (x) (equal? (x (delay 2)) 1)))
                (ep "le8" "let { i x = x } in i" (lambda (x) (procedure? x)))
                (ep "le9" "let { i x = x } in i" (lambda (x) (equal? (x (delay 1)) 1)))
                (ep "le10" "let { i x y = x } in i" (lambda (x) (procedure? x)))
                (ep "le11" "let { i x y = x } in i" (lambda (x) (procedure? (x (delay 1)))))
                (ep "le12" "let { i x y = x } in i" (lambda (x) (equal? ((x (delay 1)) (delay 2)) 1)))
                (ee "li1" "[]" (make-haskell/constructor/#Nil))
                (ee "li2" "[1]" (((force haskell/:) (delay 1)) (delay (make-haskell/constructor/#Nil))))
                (ee "li3" "[1, 2]" (((force haskell/:) (delay 1)) (delay (((force haskell/:) (delay 2)) (delay (make-haskell/constructor/#Nil))))))
                #|(ee "ml-bo1" ":ml Bool \"mlBoolean1\"" (force haskell/False))
                (ex "ml-bo2" ":ml Bool \"mlFunction1\"")
                (ex "ml-bo3" ":ml Bool \"mlInteger\"")
                (ex "ml-bo4" ":ml Bool \"mlString\"")
                (ex "ml-bo5" ":ml Bool \"mlTuple\"")
                (ex "ml-bo6" ":ml Bool \"mlUnit\"")
                (ex "ml-in1" ":ml Int \"mlBoolean1\"")
                (ex "ml-in2" ":ml Int \"mlFunction1\"")
                (ee "ml-in3" ":ml Int \"mlInteger\"" 1)
                (ex "ml-in4" ":ml Int \"mlString\"")
                (ex "ml-in5" ":ml Int \"mlTuple\"")
                (ex "ml-in6" ":ml Int \"mlUnit\"")
                (ex "ml-st1" ":ml [Char] \"mlBoolean1\"")
                (ex "ml-st2" ":ml [Char] \"mlFunction1\"")
                (ex "ml-st3" ":ml [Char] \"mlInteger\"")
                (ee "ml-st4" ":ml [Char] \"mlString\"" (cons (delay #\a) (delay (cons (delay #\b) (delay null)))))
                (ex "ml-st5" ":ml [Char] \"mlTuple\"")
                (ex "ml-st6" ":ml [Char] \"mlUnit\"")
                (ex "ml-fu1" ":ml Int -> Int \"mlBoolean1\"")
                (ep "ml-fu2" ":ml Int -> Int \"mlFunction1\"" (lambda (x) (equal? (x (delay 1)) 1)))
                (ex "ml-fu3" ":ml Int -> Int \"mlInteger\"")
                (ex "ml-fu4" ":ml Int -> Int \"mlString\"")
                (ex "ml-fu5" ":ml Int -> Int \"mlTuple\"")
                (ex "ml-fu6" ":ml Int -> Int \"mlUnit\"")
                (ep "ml-fu7"
                    ":ml Bool -> Bool \"mlFunction1\""
                    (lambda (x)
                      (equal (x (delay (force haskell/False)))
                             (force haskell/False))))
                (ep "ml-fu8"
                    ":ml ((Bool, Bool), (Bool, Bool)) -> ((Bool, Bool), (Bool, Bool)) \"mlFunction1\""
                    (lambda (x)
                      (let ((v (vector-immutable (delay (vector-immutable (delay (force haskell/True))
                                                                          (delay (force haskell/False))))
                                                 (delay (vector-immutable (delay (force haskell/True))
                                                                          (delay (force haskell/False)))))))
                        (equal (x (delay v)) v))))
                (ep "ml-fu9" ":ml () -> () \"mlFunction1\"" (lambda (x) (equal? (x (delay (make-haskell/constructor/#Unit))) (make-haskell/constructor/#Unit))))
                (ep "ml-fu10"
                    ":ml a -> a \"mlFunction1\""
                    (lambda (x)
                      (let ((s (cons (delay #\a)
                                     (delay (cons (delay #\b)
                                                  (delay null))))))
                        (equal (x (delay s)) s))))
                (ex "ml-tu1" ":ml (Bool, [Char]) \"mlBoolean1\"")
                (ex "ml-tu2" ":ml (Bool, [Char]) \"mlFunction1\"")
                (ex "ml-tu3" ":ml (Bool, [Char]) \"mlInteger\"")
                (ex "ml-tu4" ":ml (Bool, [Char]) \"mlString\"")
                (ee "ml-tu5"
                    ":ml (Bool, [Char]) \"mlTuple\""
                    (vector-immutable (delay (force haskell/False))
                                      (delay (cons (delay #\a)
                                                   (delay (cons (delay #\b)
                                                                (delay null)))))))
                (ex "ml-tu6" ":ml (Bool, [Char]) \"mlUnit\"")
                (ex "ml-un1" ":ml () \"mlBoolean1\"")
                (ex "ml-un2" ":ml () \"mlFunction1\"")
                (ex "ml-un3" ":ml () \"mlInteger\"")
                (ex "ml-un4" ":ml () \"mlString\"")
                (ex "ml-un5" ":ml () \"mlTuple\"")
                (ee "ml-un6" ":ml () \"mlUnit\"" (make-haskell/constructor/#Unit))
                (ee "sc-ch1" ":scheme Char \"schemeCharacter\"" #\a)
                (ex "sc-ch2" ":scheme Char \"schemeFloat\"")
                (ex "sc-ch3" ":scheme Char \"schemeFunction\"")
                (ex "sc-ch4" ":scheme Char \"schemeInteger\"")
                (ex "sc-ch5" ":scheme Char \"schemeList1\"")
                (ex "sc-ch6" ":scheme Char \"schemeTuple\"")
                (ex "sc-fl1" ":scheme Float \"schemeCharacter\"")
                (ee "sc-fl2" ":scheme Float \"schemeFloat\"" 1.2)
                (ex "sc-fl3" ":scheme Float \"schemeFunction\"")
                (ee "sc-fl4" ":scheme Float \"schemeInteger\"" 1)
                (ex "sc-fl5" ":scheme Float \"schemeList1\"")
                (ex "sc-fl6" ":scheme Float \"schemeTuple\"")
                (efx "sc-fu1" ":scheme a -> a \"schemeCharacter\"" (lambda (x) (x (delay 1))))
                (efx "sc-fu2" ":scheme a -> a \"schemeFloat\"" (lambda (x) (x (delay 1))))
                (ep "sc-fu3" ":scheme a -> a \"schemeFunction\"" (lambda (x) (equal? (x (delay 1)) 1)))
                (efx "sc-fu4" ":scheme a -> a \"schemeInteger\"" (lambda (x) (x (delay 1))))
                (efx "sc-fu5" ":scheme a -> a \"schemeList1\"" (lambda (x) (x (delay 1))))
                (efx "sc-fu6" ":scheme a -> a \"schemeTuple\"" (lambda (x) (x (delay 1))))
                (ep "sc-fu7" ":scheme Int -> Int -> Int \"primitive:number-add\"" (lambda (x) (equal? ((x (delay 1)) (delay 2)) 3)))
                (ep "sc-fu8" ":scheme Char -> Char \"schemeFunction\"" (lambda (x) (equal? (x (delay #\a)) #\a)))
                (ep "sc-fu9" ":scheme Float -> Float \"schemeFunction\"" (lambda (x) (equal? (x (delay 1.2)) 1.2)))
                (ep "sc-fu10" ":scheme (a -> a) -> a -> a \"schemeFunction\"" (lambda (x) (equal? ((x (delay (lambda (x) (force x)))) 1) 1)))
                (ep "sc-fu11" ":scheme Int -> Int \"schemeFunction\"" (lambda (x) (equal? (x (delay 1)) 1)))
                (ep "sc-fu12"
                    ":scheme [a] -> [a] \"schemeFunction\""
                    (lambda (x)
                      (let ((s (cons (delay #\a)
                                     (delay (cons (delay #\b)
                                                  (delay null))))))
                        (equal (x (delay s)) s))))
                (ep "sc-fu13"
                    ":scheme (a, b) -> (a, b) \"schemeFunction\""
                    (lambda (x)
                      (let ((v (vector-immutable (delay 1) (delay 2))))
                        (equal? (x (delay v)) v))))
                (ex "sc-in1" ":scheme Int \"schemeCharacter\"")
                (ex "sc-in2" ":scheme Int \"schemeFloat\"")
                (ex "sc-in3" ":scheme Int \"schemeFunction\"")
                (ee "sc-in4" ":scheme Int \"schemeInteger\"" 1)
                (ex "sc-in5" ":scheme Int \"schemeList1\"")
                (ex "sc-in6" ":scheme Int \"schemeTuple\"")
                (ex "sc-li1" ":scheme [a] \"schemeCharacter\"")
                (ex "sc-li2" ":scheme [a] \"schemeFloat\"")
                (ex "sc-li3" ":scheme [a] \"schemeFunction\"")
                (ex "sc-li4" ":scheme [a] \"schemeInteger\"")
                (ee "sc-li5" ":scheme [a] \"schemeList1\"" null)
                (ee "sc-li6" ":scheme [a] \"schemeList2\"" (cons (delay 1) (delay null)))
                (ex "sc-li7" ":scheme [a] \"schemeTuple\"")
                (ex "sc-tu1" ":scheme (Int, Int) \"schemeCharacter\"")
                (ex "sc-tu2" ":scheme (Int, Int) \"schemeFloat\"")
                (ex "sc-tu3" ":scheme (Int, Int) \"schemeFunction\"")
                (ex "sc-tu4" ":scheme (Int, Int) \"schemeInteger\"")
                (ex "sc-tu5" ":scheme (Int, Int) \"schemeList1\"")
                (ee "sc-tu6" ":scheme (Int, Int) \"schemeTuple\"" (vector-immutable (delay 1) (delay 2)))|#
                (ep "tc1" "(,)" (lambda (x) (immutable? ((x (delay 1)) (delay 2)))))
                (ep "tc2" "(,)" (lambda (x) (equal ((x (delay 1)) (delay 2)) (vector-immutable (delay 1) (delay 2)))))
                (ep "tc3" "(,,)" (lambda (x) (immutable? (((x (delay 1)) (delay 2)) (delay 3)))))
                (ep "tc4" "(,,)" (lambda (x) (equal (((x (delay 1)) (delay 2)) (delay 3)) (vector-immutable (delay 1) (delay 2) (delay 3)))))
                (ep "tu1" "(1, 2)" (lambda (x) (immutable? x)))
                (ee "tu2" "(1, 2)" (vector-immutable (delay 1) (delay 2)))
                (ep "tu3" "(1, 2, 3)" (lambda (x) (immutable? x)))
                (ee "tu4" "(1, 2, 3)" (vector-immutable (delay 1) (delay 2) (delay 3)))
                (ee "un1" "()" (make-haskell/constructor/#Unit)))))