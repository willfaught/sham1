(module Test mzscheme
  (require (planet "main.ss" ("schematics" "schemeunit.plt" 3 3))
           (planet schematics/schemeunit:3/text-ui)
           #;(rename (lib "CompilationTest.ss" "sham" "haskell") compilation testSuite)
           (rename (lib "ConversionTest.ss" "sham") conversion testSuite)
           (rename (lib "ParsingTest.ss" "sham" "haskell") parsing testSuite)
           (rename (lib "SyntaxCheckingTest.ss" "sham" "haskell") syntaxChecking testSuite)
           (rename (lib "TransformationTest.ss" "sham" "haskell") transformation testSuite)
           (rename (lib "TypeCheckingTest.ss" "sham" "haskell") typeChecking testSuite))
  
  (define (runTests)
    (run-tests (test-suite "All" #;compilation #;conversion parsing syntaxChecking transformation typeChecking))))