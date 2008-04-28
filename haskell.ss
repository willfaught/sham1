(module haskell mzscheme
  (require (only (lib "1.ss" "srfi") zip)
           (only (lib "compiler.ss" "haskell") compile-module)
           (only (lib "match.ss") match-lambda match-let*)
           (only (lib "reader.ss" "haskell") language-lexer module-parser)
           (lib "terms.ss" "haskell")
           (lib "typechecker.ss" "haskell"))
  
  (provide (rename read-haskell-syntax read-syntax))
  
  ; insert-boundary :: type -> declaration-term -> declaration-term
  (define (insert-boundary type declaration)
    (match-let* ((($ declaration-term p e) declaration)
                 (term (if (equal? (length p) 1) e (make-function-term (cdr p) e))))
      (make-declaration-term (list (car p)) (make-haskell-guard-term type (make-haskell-term type term)))))
  
  ; read-syntax :: string -> port -> datum
  (define (read-haskell-syntax source-name input-port)
    (port-count-lines! input-port)
    (match-let* ((module ((module-parser source-name) (lambda () (language-lexer input-port))))
                 (declaration-types (module-declaration-types module))
                 (declarations (map (match-lambda ((type declaration) (insert-boundary type declaration)))
                                    (zip declaration-types (module-term-declarations module)))))
      (compile-module (make-module-term (module-term-identifier module) declarations) declaration-types))))