(module ConvertersTest mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "Converters.ss" "sham")
           (lib "HaskellSyntax.ss" "sham" "haskell")
           (lib "Parsers.ss" "sham" "haskell"))
  
  (provide testSuite)
  
  ; eHM :: string string datum -> schemeunit-test-case
  (define (eHM name type datum)
    (test-equal? name (convertHM (parseT type) 'test) datum))
  
  ; eMH :: string string datum -> schemeunit-test-case
  (define (eMH name type datum)
    (test-equal? name (convertMH (parseT type) 'test) datum))
  
  ; xHM :: string string -> schemeunit-test-case
  (define (xHM name type)
    (test-exn name (lambda (x) #t) (lambda () (convertHM (parseT type) 'test))))
  
  ; xMH :: string string -> schemeunit-test-case
  (define (xMH name type)
    (test-exn name (lambda (x) #t) (lambda () (convertMH (parseT type) 'test))))
  
  ; parseT :: string -> HaskellSyntax
  (define (parseT expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (tParser (lambda () (language-lexer port)))))
  
  ; testSuite :: schemeunit-test-suite
  (define testSuite
    (test-suite "Converters"
                (test-suite "HM"
                            (eHM "co1" "Bool" '(if test (force haskell:True) (force haskell:False)))
                            (xHM "co2" "Char")
                            (eHM "co3" "Int" 'test)
                            (eHM "co4" "String" '(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list test)))
                            (eHM "va1" "a" 'test)
                            (eHM "un1" "()" 'test)
                            (eHM "fu1" "a -> a" '(lambda (x1) (test (force x1))))
                            (eHM "fu2" "Bool -> Bool" '(lambda (x1) (if (test (if (haskell:True? (force x1)) #t #f)) (force haskell:True) (force haskell:False))))
                            (eHM "fu3" "a -> a -> a" '(lambda (x1) (lambda (x2) ((test (force x1)) (force x2)))))
                            (eHM "fu4"
                                 "Bool -> Bool -> Bool"
                                 '(lambda (x1)
                                    (lambda (x2)
                                      (if ((test (if (haskell:True? (force x1)) #t #f)) (if (haskell:True? (force x2)) #t #f))
                                          (force haskell:True)
                                          (force haskell:False)))))
                            (eHM "fu5" "(a -> a) -> a" '(lambda (x1) (test (lambda (x2) ((force x1) (delay x2))))))
                            (eHM "fu6"
                                 "(Bool -> Bool) -> Bool"
                                 '(lambda (x1)
                                    (if (test (lambda (x2) (if (haskell:True? ((force x1) (delay (if x2 (force haskell:True) (force haskell:False))))) #t #f)))
                                        (force haskell:True)
                                        (force haskell:False))))
                            (eHM "li1" "[a]" '(foldr (lambda (x y) (cons (delay x) (delay y))) null test))
                            (eHM "li2" "[Bool]" '(foldr (lambda (x y) (cons (delay (if x (force haskell:True) (force haskell:False))) (delay y))) null test))
                            (eHM "tu1" "(a, b)" '((lambda (x) (vector-immutable (delay (vector-ref x 0)) (delay (vector-ref x 1)))) test))
                            (eHM "tu2" "(Bool, Bool)" '((lambda (x) (vector-immutable (delay (if (vector-ref x 0) (force haskell:True) (force haskell:False)))
                                                                                      (delay (if (vector-ref x 1) (force haskell:True) (force haskell:False))))) test)))
                (test-suite "MH"
                            (eMH "co1" "Bool" '(if (haskell:True? test) #t #f))
                            (xMH "co2" "Char")
                            (eMH "co3" "Int" 'test)
                            (eMH "va1" "a" 'test)
                            (eMH "un1" "()" 'test)
                            (eMH "fu1" "a -> a" '(lambda (x1) (test (delay x1))))
                            (eMH "fu2" "Bool -> Bool" '(lambda (x1) (if (haskell:True? (test (delay (if x1 (force haskell:True) (force haskell:False))))) #t #f)))
                            (eMH "fu3" "a -> a -> a" '(lambda (x1) (lambda (x2) ((test (delay x1)) (delay x2)))))
                            (eMH "fu4"
                                 "Bool -> Bool -> Bool"
                                 '(lambda (x1)
                                    (lambda (x2)
                                      (if (haskell:True? ((test (delay (if x1
                                                                           (force haskell:True)
                                                                           (force haskell:False))))
                                                          (delay (if x2
                                                                     (force haskell:True)
                                                                     (force haskell:False))))) #t #f))))
                            (eMH "fu5" "(a -> a) -> a" '(lambda (x1) (test (delay (lambda (x2) (x1 (force x2)))))))
                            (eMH "fu6"
                                 "(Bool -> Bool) -> Bool"
                                 '(lambda (x1)
                                    (if (haskell:True? (test (delay (lambda (x2)
                                                                      (if (x1 (if (haskell:True? (force x2)) #t #f))
                                                                          (force haskell:True)
                                                                          (force haskell:False)))))) #t #f)))
                            (eMH "li1" "[a]" 'test)
                            (eMH "tu1" "(a, b)" '((lambda (x) (vector-immutable (vector-ref x 0) (vector-ref x 1))) test))
                            (eMH "tu2" "(Bool, Bool)" '((lambda (x) (vector-immutable (if (haskell:True? (vector-ref x 0)) #t #f)
                                                                                      (if (haskell:True? (vector-ref x 1)) #t #f))) test))
                )))
  
  ; tParser :: parser
  (define tParser (type-parser "test")))