(module TypeChecking scheme
  (require (only-in (lib "1.ss" "srfi") delete-duplicates filter make-list partition unzip2)
           srfi/71;(lib "71.ss" "srfi")
           (prefix-in c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (lib "List.ss" "sham" "haskell")
           (prefix-in t/ (lib "Types.ss" "sham")))
  
  (provide dataTypes moduleTypes syntaxType typeVariables wellTyped)
  
  (provide (struct-out Assumption))
  
  (define-struct Assumption (name type) #:transparent)
  
  (define-struct Constraint (left right) #:transparent)
  
  (define-struct (Forall t/Type) (variables type) #:transparent)
  
  (define-struct Substitution (old new) #:transparent)
  
  (define variableCount 0)
  
  (define (constructorTypes dataType syntax)
    (match-let (((struct c/Constructor (n f)) syntax))
      (append (list (list n (foldr (lambda (x y) (t/make-Application (t/make-Application (t/make-Function) x) y)) dataType
                                   (map (match-lambda ((struct c/Field (_ t)) t)) f)))
                    (list (string-append "is" n)
                          (t/make-Application (t/make-Application (t/make-Function) dataType)
                                              (t/make-Constructor "Bool"))))
              (map (lambda (x) (fieldType dataType x)) f))))
  
  (define (containsType container contained)
    (cond ((equal? container contained) #t)
          ((t/Application? container) (or (containsType (t/Application-operator container) contained)
                                          (containsType (t/Application-operand container) contained)))
          (else #f)))
  
  (define dataTypes
    (match-lambda
      ((struct c/Data (n t c)) (let ((dataType (foldl (lambda (x y) (t/make-Application y (t/make-Variable x))) (t/make-Constructor n) t)))
                                  (foldl append null (map (lambda (x) (constructorTypes dataType x)) c))))))
  
  (define (fieldType dataType syntax)
    (match-let (((struct c/Field (n t)) syntax))
      (list n (t/make-Application (t/make-Application (t/make-Function) dataType) t))))
  
  (define (generalize context type)
    (let ((tyvars (uniqueTypeVariables type))
          (types (map (match-lambda ((struct Assumption (_ t)) t)) context)))
      (if (null? tyvars) type (make-Forall (map (match-lambda ((list x _) x))
                                                (filter (lambda (x) (equal? (list-ref x 1) #f))
                                                        (map (lambda (x) (list x (findf (lambda (y) (containsType y x)) types))) tyvars)))
                                           type))))
  
  (define instantiate
    (match-lambda ((struct Forall (v t)) (rename (zip v (map (lambda (x) (newVariable)) v)) t))))
  
  (define (mapType mapper type)
    (match type
      ((struct t/Application (r d)) (mapper (t/make-Application (mapType mapper r) (mapType mapper d))))
      (x (mapper x))))
  
  (define moduleTypes
    (match-lambda
      ((struct c/Module (n e i d))
       (match-let* (((list data decl) (values->list (partition c/Data? d)))
                    (names (map (match-lambda ((struct c/Declaration (n _)) n)) decl))
                    (tyvars (map (lambda (x) (newVariable)) decl))
                    (dataCxt (map (match-lambda ((list n t) (make-Assumption n (generalize null t)))) (foldl append null (map dataTypes data))))
                    (importTypes (map (match-lambda ((struct c/Import (_ m n t)) (make-Assumption (string-append m "." n) (generalize null t)))) i))
                    (declCxt (append (zipWith make-Assumption names tyvars) dataCxt importTypes))
                    ((list types constraints) (values->list (unzip2 (map (lambda (x) (reconstruct declCxt x)) decl))))
                    (subst (unify (append (zipWith make-Constraint tyvars types) (foldl append null constraints))))
                    (typesS (map (lambda (x) (substituteManyType subst x)) types)))
         (append importTypes dataCxt (zipWith make-Assumption names typesS))))))
  
  (define (newVariable)
    (set! variableCount (+ variableCount 1))
    (t/make-Variable (string-append "t" (number->string variableCount))))
  
  (define (normalize type)
    (let ((vars (uniqueTypeVariables type)))
      (rename (zip vars (map (lambda (x) (t/make-Variable (if (equal? x 0) "t" (string-append "t" (number->string x)))))
                             (iterate (lambda (x) (+ x 1)) 0 (length vars)))) type)))
  
  (define (reconstruct context syntax)
    (match syntax
      ((struct c/Application (r d)) (match-let (((list rt rc) (reconstruct context r))
                                                ((list dt dc) (reconstruct context d))
                                                (t (newVariable)))
                                      (list t (cons (make-Constraint rt (t/make-Application (t/make-Application (t/make-Function) dt) t)) (append rc dc)))))
      ((? c/Character? _) (list (t/make-Constructor "Char") null))
      ((struct c/Declaration (_ r)) (reconstruct context r))
      ((? c/Float? _) (list (t/make-Constructor "Float") null))
      ((struct c/Function (p b)) (match-let* ((pt (newVariable))
                                              ((list bt bc) (reconstruct (cons (make-Assumption p pt) context) b)))
                                   (list (t/make-Application (t/make-Application (t/make-Function) pt) bt) bc)))
      ((struct c/If (g t e)) (match-let (((list gt gc) (reconstruct context g))
                                         ((list tt tc) (reconstruct context t))
                                         ((list et ec) (reconstruct context e)))
                               (list tt (append (list (make-Constraint gt (t/make-Constructor "Bool")) (make-Constraint tt et)) gc tc ec))))
      ((? c/Integer? _) (list (t/make-Constructor "Int") null))
      ((struct c/Let (d b)) (match-let* ((names (map (match-lambda ((struct c/Declaration (l _)) l)) d))
                                         (tyvars (map (lambda (x) (newVariable)) d))
                                         ((list types constraints)
                                          (values->list (unzip2 (map (match-lambda ((struct c/Declaration (_ r))
                                                                                    (reconstruct (append (zipWith make-Assumption names tyvars) context) r))) d))))
                                         (subst (unify (append (zipWith make-Constraint tyvars types) (foldl append null constraints))))
                                         (contextS (substituteContext subst context))
                                         (typesS (map (lambda (x) (generalize contextS (substituteManyType subst x))) types)))
                              (reconstruct (append (zipWith make-Assumption names typesS) contextS) b)))
      ((struct c/ListConstructor ()) (list (t/make-Application (t/make-List) (newVariable)) null))
      ((struct c/TupleConstructor (a)) (let ((t (map (lambda (x) (newVariable)) (make-list a))))
                                         (list (foldr (lambda (x y) (t/make-Application (t/make-Application (t/make-Function) x) y))
                                                      (foldl (lambda (x y) (t/make-Application y x)) (t/make-Tuple a) t) t) null)))
      ((struct c/UnitConstructor ()) (list (t/make-Unit) null))
      ((struct c/Variable (n)) (match (findf (lambda (x) (equal? (Assumption-name x) n)) context)
                                 (#f (error (format "TypeChecking: Found a free variable '~a'" n)))
                                 (x (list (let ((t (Assumption-type x))) (if (Forall? t) (instantiate t) t)) null))))))
  
  (define (rename mappings type)
    (mapType (match-lambda ((? t/Variable? x) (match (assoc x mappings) ((list _ y) y) (#f x))) (x x)) type))
  
  (define (substituteConstraint s c)
    (make-Constraint (substituteType s (Constraint-left c))
                     (substituteType s (Constraint-right c))))
  
  (define (substituteContext s c)
    (if (null? s) c (substituteContext (cdr s) (map (match-lambda ((struct Assumption (n t)) (make-Assumption n (substituteType (car s) t)))) c))))
  
  (define (substituteType s t)
    (mapType (lambda (x) (if (equal? x (Substitution-old s)) (Substitution-new s) x)) t))
  
  (define (substituteManyType s t)
    (if (null? s) t (substituteManyType (cdr s) (substituteType (car s) t))))
  
  (define (syntaxType syntax)
    (match-let (((list t c) (reconstruct null syntax)))
      (normalize (substituteManyType (unify c) t))))
  
  (define typeVariables
    (match-lambda
      ((struct t/Application (r d)) (append (typeVariables r) (typeVariables d)))
      ((? t/Variable? x) (list x))
      (_ null)))
  
  (define unify
    (match-lambda
      ((list) null)
      ((cons (struct Constraint (t1 t2)) rest)
       (cond ((equal? t1 t2) (unify rest))
             ((and (t/Application? t1) (t/Application? t2))
              (unify (cons (make-Constraint (t/Application-operator t1) (t/Application-operator t2))
                           (cons (make-Constraint (t/Application-operand t1) (t/Application-operand t2)) rest))))
             ((and (t/Variable? t1) (not (containsType t2 t1)))
              (let ((s (make-Substitution t1 t2))) (cons s (unify (map (lambda (x) (substituteConstraint s x)) rest)))))
             ((and (t/Variable? t2) (not (containsType t1 t2)))
              (let ((s (make-Substitution t2 t1))) (cons s (unify (map (lambda (x) (substituteConstraint s x)) rest)))))
             (else (error 'unify "cannot unify ~a with ~a" t1 t2))))))
  
  (define (uniqueTypeVariables type)
    (remove-duplicates (typeVariables type)))
  
  (define (wellTyped syntax) (moduleTypes syntax) #t))
