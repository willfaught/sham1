(module reader mzscheme
  (require (lib "compiler.ss" "haskell")
           (lib "parsers.ss" "haskell"))
  
  (provide (rename read-haskell-syntax read-syntax))
  
  ; read-haskell-syntax :: string input-port -> datum
  (define (read-haskell-syntax source-name input-port)
    (port-count-lines! input-port)
    (compile-term ((module-parser source-name) (lambda () (language-lexer input-port))))))