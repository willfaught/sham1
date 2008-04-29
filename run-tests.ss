(module run-tests mzscheme
  (require (prefix typechecker. (lib "typechecker-test.ss" "haskell")))
  
  (provide run-tests)
  
  (define (run-tests)
    (append (typechecker.run-tests))))