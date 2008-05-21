(module run-tests mzscheme
  
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
           "ml-reader.ss"
           "type-tests.ss"
           "examples/through-tests.ss")
  
  (print-struct #t)
  
  (test/text-ui unification-test-suite)
  (test/text-ui simple-test-suite)
  (test/text-ui tuple-test-suite)
  (test/text-ui type-test-suite)
  (test/text-ui unit-test-suite))

