(module SyntaxCheckingTest mzscheme
  (require (lib "contract.ss")
           (planet "main.ss" ("schematics" "schemeunit.plt" 3 3)))
  
  (provide/contract (testSuite schemeunit-test-suite?))
  
  (define/contract testSuite schemeunit-test-suite?
    (test-suite "SyntaxChecking")))