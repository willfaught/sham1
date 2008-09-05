(module SyntaxTransformer mzscheme
  (require (prefix c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "types.ss" "sham")
           (lib "list.ss")
           (lib "match.ss"))
  
  (provide transformHC)
  
  ; transformHC :: HaskellSyntax -> CoreSyntax
  (define (transformHC syntax)
    (match syntax
      (($ h/Application r d) (c/make-Application (transformHC r) (transformHC d)))
      (($ h/Character v) (c/make-Character v))
      (($ h/Constructor n f) (c/make-Constructor n (foldl append null (map transformHC f))))
      (($ h/Data n c) (c/make-Data n (map transformHC c)))
      (($ h/Declaration ($ h/LHS n p) r) (c/make-Declaration n (curry p (transformHC r))))
      (($ h/Field n t) (map (lambda (x) (c/make-Field x t)) n))
      (($ h/Float v) (c/make-Float v))
      (($ h/Function p b) (curry p (transformHC b)))
      (($ h/FunctionType a r) (make-)
      (($ ) )
      (($ ) )
      (($ ) )
      (($ ) )
      (($ ) )
      (($ ) )|#))
  
  ; curry :: [string] CoreSyntax -> CoreSyntax
  (define (curry params body)
    (if (null? params) body (curry (cdr params) (c/make-Function (car params) body)))))