(module haskell mzscheme
  (require (only (lib "1.ss" "srfi") zip)
           (lib "compiler.ss" "haskell")
           (lib "list.ss" "haskell")
           (lib "match.ss")
           (lib "reader.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "typechecker.ss" "haskell"))
  
  (provide read-module (rename read-haskell-syntax read-syntax))
  
  ; insert-boundary :: type -> declaration-term -> declaration-term
  (define (insert-boundary type declaration)
    (match-let* ((($ declaration-term p e) declaration)
                 (term (if (equal? (length p) 1) e (make-function-term (cdr p) e))))
      (make-declaration-term (list (car p)) (make-haskell-guard-term type (make-haskell-term type term)))))
  
  ; read-haskell-syntax :: string -> port -> datum
  (define (read-haskell-syntax source-name input-port)
    (port-count-lines! input-port)
    (match-let* ((module ((module-parser source-name) (lambda () (language-lexer input-port))))
                 (prelude (read-module "Prelude.hs" (open-input-file "/Applications/PLT Scheme v371/collects/haskell/Prelude.hs")))
                 ((_ declaration-types) (lunzip2 (module-context (module-context null prelude) module)))
                 (declarations (map (match-lambda ((type declaration) (insert-boundary type declaration)))
                                    (zip declaration-types (module-term-declarations module)))))
      (compile-module (make-module-term (module-term-identifier module) declarations) declaration-types)))
  
  ; read-module :: string -> port -> module-term
  (define (read-module source-name input-port)
    (port-count-lines! input-port)
    (read-line input-port)
    ((module-parser source-name) (lambda () (language-lexer input-port)))))