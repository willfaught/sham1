(module Test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
           (rename (lib "ConvertersTest.ss" "sham") convertersTS testSuite)
           (rename (lib "ParsersTest.ss" "sham" "haskell") parsersTS testSuite)
           (rename (lib "SyntaxTransformerTest.ss" "sham" "haskell") syntaxTransformerTS testSuite))
  
  (define (runTests)
    (test/text-ui (test-suite "All" convertersTS parsersTS syntaxTransformerTS))))