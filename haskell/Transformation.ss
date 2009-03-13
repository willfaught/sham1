(module Transformation scheme
  (require (prefix-in c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix-in h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "Maybe.ss" "sham" "haskell")
           (lib "Parsing.ss" "sham" "haskell")
           (prefix-in t/ (lib "Types.ss" "sham"))
           (lib "TypeChecking.ss" "sham" "haskell"))
  
  (provide transformSyntax)
  
  (define (curryFunction params body)
    (if (null? params) body (c/make-Function (car params) (curryFunction (cdr params) body))))
  
  (define transformSyntax
    (match-lambda
      ((struct h/Application (r d)) (c/make-Application (transformSyntax r) (transformSyntax d)))
      ((struct h/Character (v)) (c/make-Character v))
      ((struct h/Constructor (n f)) (c/make-Constructor n (map transformSyntax f)))
      ((struct h/Data (n c)) (c/make-Data n (map transformSyntax c)))
      ((struct h/Declaration ((struct h/LHS (n p)) r)) (c/make-Declaration n (curryFunction p (transformSyntax r))))
      ((struct h/Field (n t)) (c/make-Field n t))
      ((struct h/Float (v)) (c/make-Float v))
      ((struct h/Function (p b)) (curryFunction p (transformSyntax b)))
      ((struct h/If (g t e)) (c/make-If (transformSyntax g) (transformSyntax t) (transformSyntax e)))
      ((struct h/Impdecl (l m i)) (map (match-lambda ((struct h/Import (n t)) (c/make-Import l m n t))) i))
      ((struct h/Integer (v)) (c/make-Integer v))
      ((struct h/Let (d b)) (c/make-Let (map transformSyntax d) (transformSyntax b)))
      ((struct h/List (e)) (foldr (lambda (x y) (c/make-Application (c/make-Application (c/make-Variable "Prelude.:") (transformSyntax x)) y)) (c/make-ListConstructor) e))
      ((struct h/ListConstructor ()) (c/make-ListConstructor))
      ((struct h/Module (n e i d))
       (c/make-Module n
                      (match e
                        ((struct Nothing ())
                         (let ((datas (filter h/Data? d))
                               (decls (filter h/Declaration? d)))
                           (append (map c/make-Export (map typeDeclarations datas))
                                   (map (match-lambda ((struct h/Declaration ((struct h/LHS (n _)) _)) (c/make-Export n))) decls))))
                        ((struct Just (e))
                         (map c/make-Export e)))
                      (foldl append null (map transformSyntax (cons preludeImpdecl i)))
                      (map transformSyntax d)))
      ((struct h/Tuple (e)) (foldl (lambda (x y) (c/make-Application y (transformSyntax x))) (c/make-TupleConstructor (length e)) e))
      ((struct h/TupleConstructor (a)) (c/make-TupleConstructor a))
      ((struct h/UnitConstructor ()) (c/make-UnitConstructor))
      ((struct h/Variable (n)) (c/make-Variable n))))
  
  (define typeDeclarations
    (match-lambda
      ((struct h/Data (_ c)) (map (match-lambda ((struct h/Constructor (n f)) (cons n (foldl append null (map (match-lambda ((struct h/Field (n _)) n)) f))))) c))))
  
  (define preludeTypes
    (let* ((typeParsers (parsers "Transformation"))
           (parseD (parser 'declaration typeParsers))
           (parseT (parser 'type typeParsers)))
      (append (dataTypes (transformSyntax (parseD "data Bool = True | False")))
              (list (list "Nil#" (parseT "[a]"))
                    (list "Unit#" (parseT "()"))
                    (list "error" (parseT "[Char] -> a"))
                    (list "fst" (parseT "(a, b) -> a"))
                    (list "head" (parseT "[a] -> a"))
                    (list "null" (parseT "[a] -> Bool"))
                    (list "snd" (parseT "(a, b) -> b"))
                    (list "tail" (parseT "[a] -> [a]"))
                    (list ":" (parseT "a -> [a] -> [a]"))))))
  
  (define preludeImpdecl
    (h/make-Impdecl "haskell" "Prelude" (map (match-lambda ((list n t) (h/make-Import n t))) preludeTypes))))