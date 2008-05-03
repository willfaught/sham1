(module run-tests mzscheme
  (require (prefix compiler. (lib "compiler-test.ss" "haskell"))
           (prefix typechecker. (lib "typechecker-test.ss" "haskell")))
  
  (provide run-tests)
  
  (define (run-tests)
    (list (list "compiler" (compiler.run-tests))
          (list "typechecker" (typechecker.run-tests)))))