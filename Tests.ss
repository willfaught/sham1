(module run-tests mzscheme
  (require (prefix compiler. (lib "compiler-test.ss" "haskell"))
           (prefix type-checker. (lib "type-checker-test.ss" "haskell"))
           (prefix parsers. (lib "parsers-test.ss" "haskell")))
  
  (provide run-tests)
  
  (define (run-tests)
    (list (list "compiler" (compiler.run-tests))
          (list "type-checker" (type-checker.run-tests))
          (list "parsers" (parsers.run-tests))))
  
  ; runTests :: [string]
  (define (runTests)
    (define (results x y)
      (cond ((test-failure? x) (cons (test-result-test-case-name x) y))
            ((test-error? x) (cons (test-result-test-case-name x) y))
            (else y)))
    (fold-test-results results null tests)))