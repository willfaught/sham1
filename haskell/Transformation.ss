(module Transformation scheme
  (require (prefix-in c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix-in h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (prefix-in t/ (lib "Types.ss" "sham")))
  
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
      ((struct h/Impdecl (l p i)) (map (match-lambda ((struct h/Import (n a t)) (c/make-Import l p n a t))) i))
      ((struct h/Integer (v)) (c/make-Integer v))
      ((struct h/Let (d b)) (c/make-Let (map transformSyntax d) (transformSyntax b)))
      ((struct h/List (e)) (foldr (lambda (x y) (c/make-Application (c/make-Application (c/make-Variable ":") (transformSyntax x)) y)) (c/make-ListConstructor) e))
      ((struct h/ListConstructor ()) (c/make-ListConstructor))
      ((struct h/Module (n e i d)) (c/make-Module n (map c/make-Export e) (foldl append null (map transformSyntax i)) (map transformSyntax d)))
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
      ((struct h/UnitType ()) (t/make-Unit)))))