(module Test scheme
  (require (planet schematics/schemeunit:3:3)
           (planet schematics/schemeunit:3/text-ui)
           #;(rename-in (lib "BoundaryTest.ss" "sham") (testSuite boundary))
           #;(rename-in (lib "CompilationTest.ss" "sham" "haskell") (testSuite compilation))
           (rename-in (lib "ParsingTest.ss" "sham" "haskell") (testSuite parsing))
           #;(rename-in (lib "SyntaxCheckingTest.ss" "sham" "haskell") (testSuite syntaxChecking))
           (rename-in (lib "TransformationTest.ss" "sham" "haskell") (testSuite transformation))
           (rename-in (lib "TypeCheckingTest.ss" "sham" "haskell") (testSuite typeChecking)))
  
  (define (runTests)
    (run-tests (test-suite "All" #;boundary #;compilation parsing #;syntaxChecking transformation typeChecking))))