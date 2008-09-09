(module SyntaxTransformer mzscheme
  (require (prefix c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (prefix t/ (lib "Types.ss" "sham"))
           (lib "contract.ss")
           (lib "list.ss")
           (lib "match.ss"))
  
  (provide transformHC)
  
  ; contractHS :: HaskellSyntax -> contract
  (define (contractHS type)
    (match type
      (($ h/FunctionType p r) `(-> ,(contractSH p) ,(contractHS r)))
      (($ h/ListType t) `(and/c (listof ,(contractHS t)) (flat-contract proper-list?)  (flat-contract (lambda (x) (not (circular-list? x))))))
      (($ h/TupleType t) `(vector-immutable/c ,@(map contractHS t)))
      (($ h/TypeConstructor "Char") `(flat-contract char?))
      (($ h/TypeConstructor "Float") `(flat-contract number?))
      (($ h/TypeConstructor "Int") `(flat-contract integer?))
      (($ h/TypeConstructor n) `(flat-contract ,(string->symbol (string-append "haskell-type:" n "?"))))
      ((? h/TypeVariable? _) 'any/c)
      (($ h/UnitType) `(vector-immutable/c))))
  
  ; contractSH :: HaskellSyntax -> contract
  (define (contractSH type)
    (match type
      (($ h/FunctionType p r) `(-> ,(contractHS p) ,(contractSH r)))
      ((? (lambda (x) (or h/FunctionType? h/ListType? h/TupleType? h/TypeConstructor? h/TypeVariable? h/UnitType?)) _) 'any/c)))
  
  ; transformHC :: HaskellSyntax -> CoreSyntax
  (define (transformHC syntax)
    (match syntax
      (($ h/Application r d) (c/make-Application (transformHC r) (transformHC d)))
      (($ h/Character v) (c/make-Character v))
      (($ h/Constructor n f) (c/make-Constructor n (map transformHC f)))
      (($ h/Data n c) (c/make-Data n (map transformHC c)))
      (($ h/Declaration ($ h/LHS n p) r) (c/make-Declaration n (curry p (transformHC r))))
      (($ h/Field n t) (c/make-Field n (transformHC t)))
      (($ h/Float v) (c/make-Float v))
      (($ h/Function p b) (curry p (transformHC b)))
      (($ h/FunctionType p r) (t/make-Application (t/make-Application (t/make-Function) (transformHC p)) (transformHC r)))
      (($ h/If g t e) (c/make-If (transformHC g) (transformHC t) (transformHC e)))
      (($ h/Integer v) (c/make-Integer v))
      (($ h/Let d b) (c/make-Let (map transformHC d) (transformHC b)))
      (($ h/List e) (foldr (lambda (x y) (c/make-Application (c/make-Application (c/make-Variable ":") (transformHC x)) y)) (c/make-ListConstructor) e))
      (($ h/ListConstructor) (c/make-ListConstructor))
      (($ h/ListType t) (t/make-Application (t/make-List) (transformHC t)))
      (($ h/ML t n) (c/make-ML (transformHC t) n))
      (($ h/Module n ($ h/Body i d)) (c/make-Module n i (map transformHC d)))
      (($ h/Scheme t n) (c/make-Scheme (transformHC t) (contractHS t) n))
      (($ h/Tuple e) (foldl (lambda (x y) (c/make-Application y (transformHC x))) (c/make-TupleConstructor (length e)) e))
      (($ h/TupleConstructor a) (c/make-TupleConstructor a))
      (($ h/TupleType t) (foldl (lambda (x y) (t/make-Application y (transformHC x))) (t/make-Tuple (length t)) t))
      (($ h/TypeApplication t) (foldl (lambda (x y) (t/make-Application y (transformHC x))) (transformHC (car t)) (cdr t)))
      (($ h/TypeConstructor n) (t/make-Constructor n))
      (($ h/TypeVariable n) (t/make-Variable n))
      (($ h/UnitConstructor) (c/make-UnitConstructor))
      (($ h/UnitType) (t/make-Unit))
      (($ h/Variable n) (c/make-Variable n))))
  
  ; curry :: [string] CoreSyntax -> CoreSyntax
  (define (curry params body)
    (if (null? params) body (c/make-Function (car params) (curry (cdr params) body)))))