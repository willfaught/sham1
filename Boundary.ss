(module Boundary scheme
  (require (only-in (lib "1.ss" "srfi") list-tabulate zip)
           (lib "HaskellSyntax.ss" "sham" "haskell"))
  
  (provide boundaryHH boundaryHM boundaryHS boundaryMH #;boundaryMS boundarySH #;boundarySM)
  
  (define-struct (LabelType HaskellSyntax) (name) #:transparent)
  
  (define-struct Wrapper (name value) #:transparent)
  
  (define (boundaryHM type syntax)
    #`(contract #,(contractHM type) #,(convertHM type syntax 1) 'ml 'haskell))
  
  (define (boundaryHH type syntax)
    syntax)
  
  (define (boundaryHS type syntax)
    (convertHS (label type) syntax 1))
  
  (define (boundaryMH type syntax)
    #`(contract #,(contractMH type) #,(convertMH type syntax 1) 'haskell 'ml))
  
  (define (boundarySH type syntax)
    (convertSH type syntax 1))
  
  (define contractHM
    (match-lambda
      ((struct FunctionType (p r)) #`(-> #,(contractMH p) #,(contractHM r)))
      ((struct ListType (t)) #`(and/c (listof ,(contractHM t)) proper-list? (lambda (x) (not (circular-list? x)))))
      ((struct TupleType (t)) #`(vector-immutable/c ,@(map contractHM t)))
      ((struct TypeConstructor (n)) #`,(string->symbol (string-append "import/" n "?")))
      ((struct TypeVariable (_)) #'any/c)
      ((struct UnitType ()) #'vector-immutable/c)))
  
  (define contractHS
    (match-lambda
      ((struct FunctionType (p r)) #`(-> ,(contractSH p) ,(contractHS r)))
      ((struct ListType (t)) #`(and/c (listof ,(contractHS t)) proper-list? (lambda (x) (not (circular-list? x)))))
      ((struct TupleType (t)) #`(vector-immutable/c ,@(map contractHS t)))
      ((struct TypeConstructor ("Bool")) #'boolean?)
      ((struct TypeConstructor ("Char")) #'char?)
      ((struct TypeConstructor ("Float")) #'number?)
      ((struct TypeConstructor ("Int")) #'integer?)
      ((struct TypeConstructor (n)) #`,(string->symbol (string-append "import/" n "?"))) ; TODO
      ((? TypeVariable? _) #'any/c) ; TODO
      ((struct UnitType ()) #'(vector-immutable/c))))
  
  (define contractMH
    (match-lambda
      ((struct FunctionType (p r)) #`(-> ,(contractHM p) ,(contractMH r)))
      ((struct ListType (t)) #'import/#List?)
      ((struct TupleType (t)) #`(list/c ,@(map contractMH t)))
      ((struct TypeConstructor (n)) #`(,(string->symbol (string-append "import/" n "?"))))
      ((struct TypeVariable (_)) #'any/c)
      ((struct UnitType ()) #'import/#Unit?)))
  
  (define contractMS
    (match-lambda
      ((struct FunctionType (p r)) #`(-> ,(contractSH p) ,(contractHS r)))
      ((struct ListType (t)) #`(and/c (listof ,(contractHS t)) proper-list? (lambda (x) (not (circular-list? x)))))
      ((struct TupleType (t)) #`(vector-immutable/c ,@(map contractHS t)))
      ((struct TypeConstructor ("Bool")) #'boolean?)
      ((struct TypeConstructor ("Char")) #'char?)
      ((struct TypeConstructor ("Float")) #'number?)
      ((struct TypeConstructor ("Int")) #'integer?)
      ((struct TypeConstructor (n)) #`,(string->symbol (string-append "haskell/type/" n "?"))) ; TODO
      ((? TypeVariable? _) #'any/c) ; TODO
      ((struct UnitType ()) #'(vector-immutable/c))))
  
  (define contractSH
    (match-lambda
      ((struct FunctionType (p r)) #`(-> ,(contractHS p) ,(contractSH r)))
      ((? (lambda (x) (or ListType? TupleType? TypeConstructor? TypeVariable? UnitType?)) _) #'any/c)))
  
  (define contractSM
    (match-lambda
      ((struct FunctionType (p r)) #`(-> ,(contractHS p) ,(contractSH r)))
      ((? (lambda (x) (or ListType? TupleType? TypeConstructor? TypeVariable? UnitType?)) _) #'any/c)))
  
  (define (convertHM type syntax depth)
    (match type
      ((struct FunctionType (p r)) (let ((n (name depth))) #`(lambda (,n) ,(convertHM r #`(,syntax ,(convertMH p #`(force ,n) (+ depth 1))) (+ depth 1)))))
      ((struct ListType (t)) #`(foldr (lambda (x y) (cons (delay ,(convertHM t #'x depth)) (delay y))) null ,syntax))
      ((struct TupleType (ts)) #`((lambda (x) (list ,@(map (match-lambda ((t i) #`(delay ,(convertHM t #`(list-ref x ,i) depth))))
                                                    (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      ((struct TypeConstructor ("Bool")) #`(if ,syntax (force haskell:True) (force haskell:False)))
      ((struct TypeConstructor ("Char")) (error 'convertHM "ML does not have characters")) ; TODO
      ((struct TypeConstructor ("String")) #`(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list ,syntax)))
      ((struct TypeConstructor (_)) syntax)
      ((struct TypeVariable (_)) syntax)
      ((struct UnitType ()) syntax)))
  
  (define (convertHS type syntax depth)
    (match type
      ((struct FunctionType (p r)) (let ((n (name depth))) `(lambda (,n) ,(convertHS r `(,syntax ,(convertSH p `(force ,n) (+ depth 1))) (+ depth 1)))))
      ((struct LabelType (n)) `((lambda (x) (if (and (Wrapper? x) (equal? (Wrapper-name x) ,n)) (Wrapper-value x) (error "Scheme violated parametricity"))) ,syntax))
      ((struct ListType (t)) `(foldr (lambda (x y) (cons (delay ,(convertHS t 'x)) (delay y))) null ,syntax))
      ((struct TupleType (ts)) `((lambda (x) (vector-immutable ,@(map (match-lambda ((list t i) `(delay ,(convertHS t `(vector-ref x ,i) depth))))
                                                               (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      ((struct TypeConstructor ("Bool")) `(if ,syntax (force haskell:True) (force haskell:False)))
      ((struct TypeConstructor ("String")) `(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list ,syntax)))
      ((struct TypeConstructor (_)) syntax)
      ((struct TypeVariable (n)) syntax)
      ((struct UnitType ()) syntax)))
  
  (define (convertMH type syntax depth)
    (match type
      ((struct FunctionType (p r)) (let ((n (name depth))) `(lambda (,n) ,(convertMH r `(,syntax (delay ,(convertHM p n (+ depth 1)))) (+ depth 1)))))
      ((struct ListType (_)) syntax)
      ((struct TupleType (ts)) `((lambda (x) (vector-immutable ,@(map (match-lambda ((list t i) (convertMH t `(vector-ref x ,i) (+ depth 1))))
                                                               (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      ((struct TypeConstructor ("Bool")) `(if (haskell:True? ,syntax) #t #f))
      ((struct TypeConstructor ("Char")) (error 'convertMH "ML does not have characters"))
      ((struct TypeConstructor (_)) syntax)
      ((struct TypeVariable (_)) syntax)
      ((struct UnitType ()) syntax)))
  
  (define (convertSH type syntax depth)
    (match type
      ((struct FunctionType (p r)) (let ((n (name depth))) `(lambda (,n) ,(convertSH r `(,syntax (delay ,(convertHS p n (+ depth 1)))) (+ depth 1)))))
      ((struct LabelType (n)) `(make-Wrapper ,n ,syntax))
      ((struct ListType (_)) syntax)
      ((struct TupleType (ts)) `((lambda (x) (vector-immutable ,@(map (match-lambda ((list t i) (convertSH t `(vector-ref x ,i) (+ depth 1))))
                                                               (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      ((struct TypeConstructor ("Bool")) `(if (haskell:True? ,syntax) #t #f))
      ((struct TypeConstructor (_)) syntax)
      ((struct TypeVariable (n)) syntax)
      ((struct UnitType ()) syntax)))
  
  (define (label type)
    (match type
      ((struct FunctionType (p r)) (make-FunctionType (label p) (label r)))
      ((struct ListType (t)) (make-ListType (label t)))
      ((struct TupleType (t)) (make-TupleType (map label t)))
      ((? TypeConstructor? t) t)
      ((struct TypeVariable (n)) (make-LabelType n))
      ((? UnitType? t) t)))
  
  (define (name depth)
    (string->symbol (string-append "x" (number->string depth)))))