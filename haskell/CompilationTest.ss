(module CompilationTest scheme
  (require (only-in (lib "1.ss" "srfi") #;circular-list? #;proper-list? zip)
           (planet schematics/schemeunit:3:3)
           (lib "Compilation.ss" "sham" "haskell")
           (lib "Parsing.ss" "sham" "haskell")
           (lib "Prelude.hs" "sham" "src" "Haskell")
           (lib "Transformation.ss" "sham" "haskell"))
  
  (provide testSuite)
  
  (define (ee name expression value)
    (ep name expression (lambda (x) (equal? x value))))
  
  (define (efx name expression f)
    (test-case name (check-exn (lambda (x) #t) (lambda () (f (evalE expression))))))
  
  (define (ep name syntax predicate)
    (test-case name (check-true (predicate (evalE syntax)))))
  
  (define (ex name expression)
    (efx name expression (lambda (x) x)))
  
  (define (iee name import value)
    (iep name import (lambda (x) (equal? x value))))
  
  (define (iep name import predicate)
    (test-case name (check-true (predicate import))))
  
  (define (evalE expression)
    (eval (datum->syntax #f (compileSyntax (transformSyntax (parseE expression))) #f)))
  
  (define (equal x y)
    (cond ((and (promise? x) (promise? y)) (equal (force x) (force y)))
          ((and (Cons#? x) (haskell/constructor/#Cons? y))
           (and (equal (force (haskell/constructor/#Cons-head x))
                       (force (haskell/constructor/#Cons-head y)))
                (equal (force (haskell/constructor/#Cons-tail x))
                       (force (haskell/constructor/#Cons-tail y)))))
          ((and (vector? x) (vector? y) (equal? (vector-length x) (vector-length y)))
           (foldl (lambda (x y) (and y (equal (list-ref x 0) (list-ref x 1)))) #t (zip (vector->list x) (vector->list y))))
          (else (equal? x y))))
  
  (define testParsers (parsers "CompilationTest"))
  
  (define parseE (parser 'expression testParsers))
  
  (define testSuite
    (test-suite "CompilationTest"
                (ee "ap1" "(\\x -> x) 1" 1)
                (ee "ap2" "(\\x y -> x) 1 2" 1)
                (ex "ap3" "1 2")
                (ex "ap4" "(\\x -> x) 1 2")
                (ep "ap5" "(\\x y -> x) 1" (lambda (x) (equal? (x (delay 2)) 1)))
                (ee "ch1" "'a'" #\a)
                #|(dp "da1"
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
                    `(lambda () (equal? ((force haskell/c) (delay ((force haskell/B) (delay (force haskell/D))))) (force haskell/D))))|#
                (ee "fl1" "1.2" 1.2)
                (ep "fu1" "\\x -> x" (lambda (x) (equal? (x (delay 1)) 1)))
                (efx "fu2" "\\x -> x" (lambda (x) ((x (delay 1)) (delay 2))))
                (ep "fu3" "\\x y -> x" (lambda (x) (equal? ((x (delay 1)) (delay 2)) 1)))
                (efx "fu4" "\\x y -> x" (lambda (x) (((x (delay 1)) (delay 2)) (delay 3))))
                (ex "id1" "x")
                (ep "id2" "fst" (lambda (x) (equal? (x (delay (list (delay 1) (delay 2)))) 1)))
                (ep "id3" "head" (lambda (x) (equal? (x (delay (make-haskell/constructor/#Cons (delay 1) (delay (make-haskell/constructor/#Nil))))) 1)))
                (ep "id4" "isFalse" (lambda (x) (equal (x (delay (force haskell/False))) (force haskell/True))))
                (ep "id5" "isFalse" (lambda (x) (equal (x (delay (force haskell/True))) (force haskell/False))))
                (ep "id6" "isTrue" (lambda (x) (equal (x (delay (force haskell/False))) (force haskell/False))))
                (ep "id7" "isTrue" (lambda (x) (equal (x (delay (force haskell/True))) (force haskell/True))))
                (ep "id8" "null" (lambda (x) (equal? (x (delay (make-haskell/constructor/#Nil))) (force haskell/True))))
                (ep "id9" "null" (lambda (x) (equal? (x (delay (cons (delay 1) (delay null)))) (force haskell/False))))
                (ep "id10" "snd" (lambda (x) (equal? (x (delay (list (delay 1) (delay 2)))) 2)))
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
                (ep "tc1" "(,)" (lambda (x) (immutable? ((x (delay 1)) (delay 2)))))
                (ep "tc2" "(,)" (lambda (x) (equal ((x (delay 1)) (delay 2)) (vector-immutable (delay 1) (delay 2)))))
                (ep "tc3" "(,,)" (lambda (x) (immutable? (((x (delay 1)) (delay 2)) (delay 3)))))
                (ep "tc4" "(,,)" (lambda (x) (equal (((x (delay 1)) (delay 2)) (delay 3)) (vector-immutable (delay 1) (delay 2) (delay 3)))))
                (ep "tu1" "(1, 2)" (lambda (x) (immutable? x)))
                (ee "tu2" "(1, 2)" (vector-immutable (delay 1) (delay 2)))
                (ep "tu3" "(1, 2, 3)" (lambda (x) (immutable? x)))
                (ee "tu4" "(1, 2, 3)" (vector-immutable (delay 1) (delay 2) (delay 3)))
                (ee "un1" "()" (make-haskell/constructor/#Unit)))))