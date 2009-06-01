(module Boundary scheme
  (require (lib "Types.ss" "sham"))
  
  (provide boundaryHH #;boundaryHM #;boundaryHS #;boundaryMH #;boundaryMS #;boundarySH #;boundarySM contractH)
  
  (define variableBindings
    (match-lambda ((struct Variable (n)) `((list ,(string->symbol (string-append "haskell/wrap/" n))
                                                 ,(string->symbol (string-append "haskell/unwrap/" n))
                                                 ,(string->symbol (string-append "haskell/wrapped/" n)))
                                           (make-haskell/Coffer)))))
  
  (define (boundaryH type syntax)
    (let ((tyvars (remove-duplicates (variables type)))
          (cont `(contract ,(contractH type) ,syntax 'ThatHaskell 'ThisHaskell)))
      (match tyvars
        ((list) cont)
        (_ `((lambda () (match-let ,(map variableBindings tyvars) ,cont)))))))
  
  (define (boundaryHH type)
    `(lambda (x) (contract ,(contractHH type) x 'ThatHaskell 'ThisHaskell)))
  
  #;(define (boundaryHM type syntax)
      #`(contract #,(contractHM type) #,(convertHM type syntax 1) 'ml 'haskell))
  
  #;(define (boundaryHS type syntax)
      (convertHS (label type) syntax 1))
  
  #;(define (boundaryMH type syntax)
      #`(contract #,(contractMH type) #,(convertMH type syntax 1) 'haskell 'ml))
  
  #;(define (boundarySH type syntax)
      (convertSH type syntax 1))
  
  (define contractH
    (match-lambda
      ((struct Application ((struct Application ((struct Function ()) p)) r))
       (let ((tyvarContract (lambda (x) (string->symbol (string-append "haskell/" x)))))
         (match (list p r)
           ((list (struct Variable (p)) (struct Variable (r))) `(-> ,(tyvarContract p) ,(tyvarContract r)))
           ((list (struct Variable (p)) r) `(-> ,(tyvarContract p) ,(contractH r)))
           ((list p (struct Variable (r))) `(-> ,(contractH p) ,(tyvarContract r)))
           ((list p r) `(-> ,(contractH p) ,(contractH r))))))
      ((struct Application (r d)) `(,(contractH r) ,(contractH d)))
      ((struct Constructor (n)) (string->symbol (string-append "type/haskell/" n)))
      ((struct List ()) 'type/haskell/Haskell.Prelude.List#)
      ((struct Tuple (a)) `(type/haskell/Haskell.Prelude.Tuple# ,a))
      ((struct Unit ()) 'type/Haskell/Haskell.Prelude.Unit#)
      ((struct Variable (n)) (string->symbol (string-append "haskell/" n)))))
  
  (define contractHH
    (match-lambda
      ((struct Application (r d)) `(,(contractHH r) (contractHH d)))
      ((struct Constructor (n)) (string->symbol (string-append "type/haskell/" n)))
      ((struct Function ()) '->)
      ((struct List ()) 'type/haskell/Haskell.Prelude.List#)
      ((struct Tuple (a)) `(type/haskell/Haskell.Prelude.Tuple# ,a))
      ((struct Unit ()) 'type/haskell/Haskell.Prelude.Unit#)
      ((struct Variable (_)) 'any/c)))
  
  (define contractHM any/c
    #;(match-lambda
        ((struct Application (r d)) `(,(contractMH r) (contractHM d)))
        ((struct Constructor (n)) (string->symbol (string-append "type/" n)))
        ((struct Function ()) 'Haskell.Function#?)
        ((struct List ()) 'Haskell.List#?)
        ((struct Tuple (a)) `(Haskell.Tuple#? ,a))
        ((struct Unit ()) 'Haskell.Unit#?)
        ((struct Variable (_)) 'any/c))
    #;(match-lambda
        ((struct FunctionType (p r)) #`(-> #,(contractMH p) #,(contractHM r)))
        ((struct ListType (t)) #`(and/c (listof ,(contractHM t)) proper-list? (lambda (x) (not (circular-list? x)))))
        ((struct TupleType (t)) #`(vector-immutable/c ,@(map contractHM t)))
        ((struct TypeConstructor (n)) #`,(string->symbol (string-append "import/" n "?")))
        ((struct TypeVariable (_)) #'any/c)
        ((struct UnitType ()) #'vector-immutable/c)))
  
  (define contractHS any/c
    #;(match-lambda
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
  
  (define contractMH any/c
    #;(match-lambda
        ((struct FunctionType (p r)) #`(-> ,(contractHM p) ,(contractMH r)))
        ((struct ListType (t)) #'import/#List?)
        ((struct TupleType (t)) #`(list/c ,@(map contractMH t)))
        ((struct TypeConstructor (n)) #`(,(string->symbol (string-append "import/" n "?"))))
        ((struct TypeVariable (_)) #'any/c)
        ((struct UnitType ()) #'import/#Unit?)))
  
  (define contractMS any/c
    #;(match-lambda
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
  
  (define contractSH any/c
    #;(match-lambda
        ((struct FunctionType (p r)) #`(-> ,(contractHS p) ,(contractSH r)))
        ((? (lambda (x) (or ListType? TupleType? TypeConstructor? TypeVariable? UnitType?)) _) #'any/c)))
  
  (define contractSM any/c
    #;(match-lambda
        ((struct FunctionType (p r)) #`(-> ,(contractHS p) ,(contractSH r)))
        ((? (lambda (x) (or ListType? TupleType? TypeConstructor? TypeVariable? UnitType?)) _) #'any/c)))
  
  (define (convertHM type syntax depth) syntax
    #;(match type
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
  
  (define (convertHS type syntax depth) syntax
    #;(match type
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
  
  (define (convertMH type syntax depth) syntax
    #;(match type
        ((struct FunctionType (p r)) (let ((n (name depth))) `(lambda (,n) ,(convertMH r `(,syntax (delay ,(convertHM p n (+ depth 1)))) (+ depth 1)))))
        ((struct ListType (_)) syntax)
        ((struct TupleType (ts)) `((lambda (x) (vector-immutable ,@(map (match-lambda ((list t i) (convertMH t `(vector-ref x ,i) (+ depth 1))))
                                                                        (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
        ((struct TypeConstructor ("Bool")) `(if (haskell:True? ,syntax) #t #f))
        ((struct TypeConstructor ("Char")) (error 'convertMH "ML does not have characters"))
        ((struct TypeConstructor (_)) syntax)
        ((struct TypeVariable (_)) syntax)
        ((struct UnitType ()) syntax)))
  
  (define (convertSH type syntax depth) syntax
    #;(match type
        ((struct FunctionType (p r)) (let ((n (name depth))) `(lambda (,n) ,(convertSH r `(,syntax (delay ,(convertHS p n (+ depth 1)))) (+ depth 1)))))
        ((struct LabelType (n)) `(make-Wrapper ,n ,syntax))
        ((struct ListType (_)) syntax)
        ((struct TupleType (ts)) `((lambda (x) (vector-immutable ,@(map (match-lambda ((list t i) (convertSH t `(vector-ref x ,i) (+ depth 1))))
                                                                        (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
        ((struct TypeConstructor ("Bool")) `(if (haskell:True? ,syntax) #t #f))
        ((struct TypeConstructor (_)) syntax)
        ((struct TypeVariable (n)) syntax)
        ((struct UnitType ()) syntax)))
  
  (define (label type) type
    #;(match type
        ((struct FunctionType (p r)) (make-FunctionType (label p) (label r)))
        ((struct ListType (t)) (make-ListType (label t)))
        ((struct TupleType (t)) (make-TupleType (map label t)))
        ((? TypeConstructor? t) t)
        ((struct TypeVariable (n)) (make-LabelType n))
        ((? UnitType? t) t)))
  
  (define (name depth)
    (string->symbol (string-append "x" (number->string depth)))))