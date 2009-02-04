(module Test scheme
  (require (planet "main.ss" ("schematics" "schemeunit.plt" 3 3))
           (planet schematics/schemeunit:3/text-ui)
           (rename-in (lib "CompilationTest.ss" "sham" "haskell") (testSuite compilation))
           #;(rename-in (lib "ConversionTest.ss" "sham") (testSuite conversion))
           (rename-in (lib "ParsingTest.ss" "sham" "haskell") (testSuite parsing))
           #;(rename-in (lib "SyntaxCheckingTest.ss" "sham" "haskell") (testSuite syntaxChecking))
           (rename-in (lib "TransformationTest.ss" "sham" "haskell") (testSuite transformation))
           (rename-in (lib "TypeCheckingTest.ss" "sham" "haskell") (testSuite typeChecking)))
  
  (define (runTests)
    (run-tests (test-suite "All" compilation #;conversion parsing #;syntaxChecking transformation typeChecking)))
  
  (runTests))