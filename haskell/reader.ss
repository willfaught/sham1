(module Reader scheme
  (require (lib "Compilation.ss" "sham" "haskell")
           (lib "List.ss" "sham" "haskell")
           (lib "Maybe.ss" "sham" "haskell")
           (lib "Parsing.ss" "sham" "haskell")
           (lib "Transformation.ss" "sham" "haskell")
           (lib "TypeChecking.ss" "sham" "haskell"))
  
  (provide (rename-out (readHaskellSyntax read-syntax)))
  
  (define (readHaskellSyntax inputName inputPort)
    (port-count-lines! inputPort)
    (let* ((parser (Just-value (lookup 'module (parsers inputName))))
           (haskellSyntax (parser (lambda () (languageLexer inputPort))))
           (coreSyntax (transformSyntax haskellSyntax))
           (types (moduleTypes coreSyntax))
           (emit (compileModule coreSyntax)))
      (pretty-print emit)
      emit)))