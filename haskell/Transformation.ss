(module Transformation scheme
  (require (prefix-in c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix-in h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "Parsing.ss" "sham" "haskell")
           (prefix-in t/ (lib "Types.ss" "sham"))
           (lib "TypeChecking.ss" "sham" "haskell"))
  
  (provide transformSyntax transformType)
  
  (define (curryFunction params body)
    (if (null? params) body (c/make-Function (car params) (curryFunction (cdr params) body))))
  
  (define transformSyntax
    (match-lambda
      ((struct h/Application (r d)) (c/make-Application (transformSyntax r) (transformSyntax d)))
      ((struct h/Character (v)) (c/make-Character v))
      ((struct h/Constructor (n f)) (c/make-Constructor n (map transformSyntax f)))
      ((struct h/Data (n c)) (c/make-Data n (map transformSyntax c)))
      ((struct h/Declaration ((struct h/LHS (n p)) r)) (c/make-Declaration n (curryFunction p (transformSyntax r))))
      ((struct h/Field (n t)) (c/make-Field n (transformType t)))
      ((struct h/Float (v)) (c/make-Float v))
      ((struct h/Function (p b)) (curryFunction p (transformSyntax b)))
      ((struct h/If (g t e)) (c/make-If (transformSyntax g) (transformSyntax t) (transformSyntax e)))
      ((struct h/Impdecl (l p ma i)) (map (match-lambda ((struct h/Import (n da t)) (c/make-Import l p ma n da t))) i))
      ((struct h/Integer (v)) (c/make-Integer v))
      ((struct h/Let (d b)) (c/make-Let (map transformSyntax d) (transformSyntax b)))
      ((struct h/List (e)) (foldr (lambda (x y) (c/make-Application (c/make-Application (c/make-Variable ":") (transformSyntax x)) y)) (c/make-ListConstructor) e))
      ((struct h/ListConstructor ()) (c/make-ListConstructor))
      ((struct h/Module (n e i d)) (c/make-Module n (map c/make-Export e) (foldl append null (map transformSyntax (cons preludeImpdecl i))) (map transformSyntax d)))
      ((struct h/Tuple (e)) (foldl (lambda (x y) (c/make-Application y (transformSyntax x))) (c/make-TupleConstructor (length e)) e))
      ((struct h/TupleConstructor (a)) (c/make-TupleConstructor a))
      ((struct h/UnitConstructor ()) (c/make-UnitConstructor))
      ((struct h/Variable (n)) (c/make-Variable n))))
  
  (define transformType
    (match-lambda
      ((struct h/FunctionType (p r)) (t/make-Application (t/make-Application (t/make-Function) (transformType p)) (transformType r)))
      ((struct h/ListType (t)) (t/make-Application (t/make-List) (transformType t)))
      ((struct h/TupleType (t)) (foldl (lambda (x y) (t/make-Application y (transformType x))) (t/make-Tuple (length t)) t))
      ((struct h/TypeApplication (t)) (foldl (lambda (x y) (t/make-Application y (transformType x))) (transformType (car t)) (cdr t)))
      ((struct h/TypeConstructor (n)) (t/make-Constructor n))
      ((struct h/TypeVariable (n)) (t/make-Variable n))
      ((struct h/UnitType ()) (t/make-Unit))))
  
  (define preludeTypes
    (let* ((typeParsers (parsers "TypeChecking"))
           (parseD (parser 'declaration typeParsers))
           (parseT (parser 'type typeParsers)))
      (append (dataTypes (transformSyntax (parseD "data Bool = True | False")))
              (list (list "Nil#" (transformType (parseT "[a]")))
                    (list "Unit#" (transformType (parseT "()")))
                    (list "error" (transformType (parseT "[Char] -> a")))
                    (list "fst" (transformType (parseT "(a, b) -> a")))
                    (list "head" (transformType (parseT "[a] -> a")))
                    (list "null" (transformType (parseT "[a] -> Bool")))
                    (list "snd" (transformType (parseT "(a, b) -> b")))
                    (list "tail" (transformType (parseT "[a] -> [a]")))
                    (list ":" (transformType (parseT "a -> [a] -> [a]")))))))
  
  (define preludeImpdecl
    (h/make-Impdecl "haskell" (list "Haskell" "Prelude.hs") "Prelude" (map (match-lambda ((list n t) (h/make-Import n n t))) preludeTypes))))