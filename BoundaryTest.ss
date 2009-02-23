(module BoundaryTest scheme
  (require (planet schematics/schemeunit:3:3)
           (lib "Boundary.ss" "sham")
           (lib "Compilation.ss" "sham" "haskell")
           (lib "HaskellSyntax.ss" "sham" "haskell")
           (lib "Parsing.ss" "sham" "haskell")
           (lib "Prelude.hs" "sham" "src" "Haskell")
           (lib "Transformation.ss" "sham" "haskell"))
  
  (provide testSuite)
  
  (define (boundaryEqual boundary name type syntax answer)
    (boundaryPredicate boundary name type syntax (lambda (x) (equal? x answer))))
  
  (define (boundaryException boundary name type syntax)
    (test-case name (check-exn (lambda (x) #t) (lambda () (eval (boundary type (compileE syntax)))))))
  
  (define (boundaryPredicate boundary name type syntax predicate)
    (test-case name (check-true (predicate (eval (boundary type (compileE syntax)))))))
  
  (define (compileE syntax)
    (compileSyntax (transformSyntax (parseE syntax))))
  
  (define eHM (curry boundaryEqual boundaryHM))
  
  #;(define eHS (curry boundaryEqual boundaryHS))
  
  (define eMH (curry boundaryEqual boundaryMH))
  
  #;(define eMS (curry boundaryEqual boundaryMS))
  
  #;(define eSH (curry boundaryEqual boundarySH))
  
  #;(define eSM (curry boundaryEqual boundarySM))
  
  (define pHM (curry boundaryPredicate boundaryHM))
  
  (define pMH (curry boundaryPredicate boundaryMH))
  
  (define xHM (curry boundaryException boundaryHM))
  
  (define xMH (curry boundaryException boundaryMH))
  
  (define testParsers (parsers "BoundaryTest"))
  
  (define parseE (parser 'expression testParsers))
  
  (define parseT (parser 'type testParsers))
  
  (define testSuite
    (test-suite "BoundaryTest"
                (test-suite "HM"
                            (eHM "bo1" "Bool" "True" (force haskell/True))
                            
                            #|(eHM "co1" "Bool" '(if test (force haskell/True) (force haskell/False)))
                            (xHM "co2" "Char")
                            (eHM "co3" "Int" 'test)
                            (eHM "co4" "String" '(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list test)))
                            (eHM "va1" "a" 'test)
                            (eHM "un1" "()" 'test)
                            (eHM "fu1" "a -> a" '(lambda (x1) (test (force x1))))
                            (eHM "fu2" "Bool -> Bool" '(lambda (x1) (if (test (if (haskell/True? (force x1)) #t #f)) (force haskell/True) (force haskell/False))))
                            (eHM "fu3" "a -> a -> a" '(lambda (x1) (lambda (x2) ((test (force x1)) (force x2)))))
                            (eHM "fu4"
                                 "Bool -> Bool -> Bool"
                                 '(lambda (x1)
                                    (lambda (x2)
                                      (if ((test (if (haskell/True? (force x1)) #t #f)) (if (haskell/True? (force x2)) #t #f))
                                          (force haskell/True)
                                          (force haskell/False)))))
                            (eHM "fu5" "(a -> a) -> a -> a" '(lambda (x1) (lambda (x2) ((test (lambda (x2) ((force x1) (delay x2)))) (force x2)))))
                            (eHM "fu6"
                                 "(Bool -> Bool) -> Bool"
                                 '(lambda (x1)
                                    (if (test (lambda (x2) (if (haskell/True? ((force x1) (delay (if x2 (force haskell/True) (force haskell/False))))) #t #f)))
                                        (force haskell/True)
                                        (force haskell/False))))
                            (eHM "li1" "[a]" '(foldr (lambda (x y) (cons (delay x) (delay y))) null test))
                            (eHM "li2" "[Bool]" '(foldr (lambda (x y) (cons (delay (if x (force haskell/True) (force haskell/False))) (delay y))) null test))
                            (eHM "tu1" "(a, b)" '((lambda (x) (vector-immutable (delay (vector-ref x 0)) (delay (vector-ref x 1)))) test))
                            (eHM "tu2" "(Bool, Bool)" '((lambda (x) (vector-immutable (delay (if (vector-ref x 0) (force haskell/True) (force haskell/False)))
                                                                                      (delay (if (vector-ref x 1) (force haskell/True) (force haskell/False))))) test))|#)
                #;(test-suite "MH"
                              (eMH "co1" "Bool" '(if (haskell/True? test) #t #f))
                              (xMH "co2" "Char")
                              (eMH "co3" "Int" 'test)
                              (eMH "va1" "a" 'test)
                              (eMH "un1" "()" 'test)
                              (eMH "fu1" "a -> a" '(lambda (x1) (test (delay x1))))
                              (eMH "fu2" "Bool -> Bool" '(lambda (x1) (if (haskell/True? (test (delay (if x1 (force haskell/True) (force haskell/False))))) #t #f)))
                              (eMH "fu3" "a -> a -> a" '(lambda (x1) (lambda (x2) ((test (delay x1)) (delay x2)))))
                              (eMH "fu4"
                                   "Bool -> Bool -> Bool"
                                   '(lambda (x1)
                                      (lambda (x2)
                                        (if (haskell/True? ((test (delay (if x1 (force haskell/True) (force haskell/False))))
                                                            (delay (if x2 (force haskell/True) (force haskell/False))))) #t #f))))
                              (eMH "fu5" "(a -> a) -> a -> a" '(lambda (x1) (lambda (x2) ((test (delay (lambda (x2) (x1 (force x2))))) (delay x2)))))
                              (eMH "fu6"
                                   "(Bool -> Bool) -> Bool"
                                   '(lambda (x1)
                                      (if (haskell/True? (test (delay (lambda (x2)
                                                                        (if (x1 (if (haskell/True? (force x2)) #t #f))
                                                                            (force haskell/True)
                                                                            (force haskell/False)))))) #t #f)))
                              (eMH "li1" "[a]" 'test)
                              (eMH "tu1" "(a, b)" '((lambda (x) (vector-immutable (vector-ref x 0) (vector-ref x 1))) test))
                              (eMH "tu2" "(Bool, Bool)" '((lambda (x) (vector-immutable (if (haskell/True? (vector-ref x 0)) #t #f)
                                                                                        (if (haskell/True? (vector-ref x 1)) #t #f))) test)))
                #;(test-suite "HS"
                              (eHS "co1" "Bool" '(if test (force haskell/True) (force haskell/False)))
                              (eHS "co2" "String" '(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list test)))
                              (eHS "co3" "Int" 'test)
                              (eHS "va1" "a" '((lambda (x)
                                                 (if (and (Wrapper? x) (equal? (Wrapper-name x) "a"))
                                                     (Wrapper-value x)
                                                     (error "Scheme violated parametricity"))) test))
                              (eHS "un1" "()" 'test)
                              (eHS "fu1"
                                   "a -> a"
                                   '(lambda (x1)
                                      ((lambda (x)
                                         (if (and (Wrapper? x) (equal? (Wrapper-name x) "a"))
                                             (Wrapper-value x)
                                             (error "Scheme violated parametricity")))
                                       (test (make-Wrapper "a" (force x1))))))
                              (eHS "fu2" "Bool -> Bool" '(lambda (x1) (if (test (if (haskell/True? (force x1)) #t #f)) (force haskell/True) (force haskell/False))))
                              (eHS "fu3"
                                   "a -> b -> a"
                                   '(lambda (x1)
                                      (lambda (x2)
                                        ((lambda (x)
                                           (if (and (Wrapper? x) (equal? (Wrapper-name x) "a"))
                                               (Wrapper-value x)
                                               (error "Scheme violated parametricity")))
                                         ((test (make-Wrapper "a" (force x1))) (make-Wrapper "b" (force x2)))))))
                              (eHS "fu4"
                                   "Bool -> Bool -> Bool"
                                   '(lambda (x1)
                                      (lambda (x2)
                                        (if ((test (if (haskell/True? (force x1)) #t #f)) (if (haskell/True? (force x2)) #t #f))
                                            (force haskell/True)
                                            (force haskell/False)))))
                              (eHS "fu5"
                                   "(a -> a) -> a -> a"
                                   '(lambda (x1)
                                      (lambda (x2)
                                        ((lambda (x)
                                           (if (and (Wrapper? x) (equal? (Wrapper-name x) "a"))
                                               (Wrapper-value x)
                                               (error "Scheme violated parametricity")))
                                         ((test (lambda (x2) ((force x1) (delay x2)))) (make-Wrapper "a" x2))))))
                              (eHS "fu6"
                                   "(Bool -> Bool) -> Bool"
                                   '(lambda (x1)
                                      (if (test (lambda (x2) (if (haskell/True? ((force x1) (delay (if x2 (force haskell/True) (force haskell/False))))) #t #f)))
                                          (force haskell/True)
                                          (force haskell/False))))
                              (eHS "li1" "[a]" '(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list test)))
                              (eHS "li2" "[Bool]" '(foldr (lambda (x y) (cons (delay (if x (force haskell/True) (force haskell/False))) (delay y))) null (string->list test)))
                              (eHS "tu1" "(a, b)" '((lambda (x) (vector-immutable (vector-ref x 0) (vector-ref x 1))) test))
                              (eHS "tu2" "(Bool, Bool)" '((lambda (x) (vector-immutable (if (vector-ref x 0) (force haskell/True) (force haskell/False))
                                                                                        (if (vector-ref x 1) (force haskell/True) (force haskell/False)))) test)))
                #;(test-suite "SH"
                              (eSH "co1" "Bool" '(if (haskell/True? test) #t #f))
                              
                              (eSH "fuX"
                                   "a -> a"
                                   '(lambda (x1)
                                      (make-Wrapper "a" (test (delay ((lambda (x)
                                                                        (if (and (Wrapper? x) (equal? (Wrapper-name x) "a"))
                                                                            (Wrapper-value x)
                                                                            (error "Scheme violated parametricity"))) x1))))))
                              ))))