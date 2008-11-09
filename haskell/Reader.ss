(module Reader mzscheme
  (require (lib "Compiler.ss" "sham" "haskell")
           (lib "SyntaxTransformer.ss" "sham" "haskell")
           (lib "Parsers.ss" "sham" "haskell"))
  
  (provide (rename readHS read-syntax))
  
  ; readHS :: string input-port -> datum
  (define (readHS inputName inputPort)
    (port-count-lines! inputPort)
    (let ((cs (transformHC ((moduleParser inputName) (lambda () (languageLexer inputPort))))))
      (display cs)
      (if (wellTyped cs)
          (compileCS cs)
          (error "Compiler: Program is ill-typed")))))