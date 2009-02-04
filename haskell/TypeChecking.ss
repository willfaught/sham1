(module TypeChecking scheme
  (require (only-in (lib "1.ss" "srfi") delete-duplicates filter make-list partition unzip2)
           (lib "71.ss" "srfi")
           (prefix-in c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (lib "List.ss" "sham" "haskell")
           (lib "Transformation.ss" "sham" "haskell")
           (lib "Parsing.ss" "sham" "haskell")
           (prefix-in t/ (lib "Types.ss" "sham")))
  
  (provide moduleTypes syntaxType wellTyped)
  
  (define-struct Assumption (name type) #:transparent)
  
  (define-struct Constraint (left right) #:transparent)
  
  (define-struct (Forall t/Type) (variables type) #:transparent)
  
  (define-struct Substitution (old new) #:transparent)
  
  (define variableCount 0)
  
  (define (constructorContext dataType syntax)
    (match-let (((struct c/Constructor (n f)) syntax))
      (append (list (make-Assumption n (foldr (lambda (x y) (t/make-Application (t/make-Application (t/make-Function) x) y)) dataType
                                              (map (match-lambda ((struct c/Field (_ t)) t)) f)))
                    (make-Assumption (string-append "is" n)
                                     (t/make-Application (t/make-Application (t/make-Function) dataType)
                                                         (t/make-Constructor "Bool"))))
              (map (lambda (x) (fieldContext dataType x)) f))))
  
  (define (containsType container contained)
    (cond ((equal? container contained) #t)
          ((t/Application? container) (or (containsType (t/Application-operator container) contained)
                                          (containsType (t/Application-operand container) contained)))
          (else #f)))
  
  (define dataContext
    (match-lambda
      ((struct c/Data (n c)) (foldl append null (map (lambda (x) (constructorContext (t/make-Constructor n) x)) c)))))
  
  (define (fieldContext dataType syntax)
    (match-let (((struct c/Field (n t)) syntax))
      (make-Assumption n (t/make-Application (t/make-Application (t/make-Function) dataType) t))))
  
  (define (generalize context type)
    (let ((tyvars (uniqueTypeVariables type))
          (types (map (match-lambda ((struct Assumption (_ t)) t)) context)))
      (if (null? tyvars) type (make-Forall (map (match-lambda ((list x _) x))
                                                (filter (lambda (x) (equal? (list-ref x 1) #f))
                                                        (map (lambda (x) (list x (findf (lambda (y) (containsType y x)) types))) tyvars))) type))))
  
  (define instantiate
    (match-lambda ((struct Forall (v t)) (rename (zip v (map (lambda (x) (newVariable)) v)) t))))
  
  (define (mapType mapper type)
    (match type
      ((struct t/Application (r d)) (mapper (t/make-Application (mapType mapper r) (mapType mapper d))))
      (x (mapper x))))
  
  (define (moduleTypes context syntax)
    (match-let* (((list data decl) (values->list (partition c/Data? (c/Module-declarations syntax))))
                 (names (map (match-lambda ((struct c/Declaration (n _)) n)) decl))
                 (tyvars (map (lambda (x) (newVariable)) decl))
                 (dataCxt (foldl append null (map dataContext data)))
                 (declCxt (append (zipWith make-Assumption names tyvars) dataCxt context primitives))
                 ((list types constraints) (values->list (unzip2 (map (lambda (x) (reconstructType declCxt x)) decl))))
                 (subst (unify (append (zipWith make-Constraint tyvars types) (foldl append null constraints))))
                 (typesS (map (lambda (x) (substituteManyType subst x)) types)))
      (append (zipWith make-Assumption names typesS) (append dataCxt context primitives))))
  
  (define (newVariable)
    (set! variableCount (+ variableCount 1))
    (t/make-Variable (string-append "t" (number->string variableCount))))
  
  (define (normalize type)
    (let ((vars (uniqueTypeVariables type)))
      (rename (zip vars (map (lambda (x) (t/make-Variable (if (equal? x 0) "t" (string-append "t" (number->string x)))))
                             (iterate (lambda (x) (+ x 1)) 0 (length vars)))) type)))
  
  (define (reconstructType context syntax)
    (match syntax
      ((struct c/Application (r d)) (match-let (((list rt rc) (reconstructType context r))
                                         ((list dt dc) (reconstructType context d))
                                         (t (newVariable)))
                               (list t (cons (make-Constraint rt (t/make-Application (t/make-Application (t/make-Function) dt) t)) (append rc dc)))))
      ((? c/Character? _) (list (t/make-Constructor "Char") null))
      ((struct c/Declaration (_ r)) (reconstructType context r))
      ((? c/Float? _) (list (t/make-Constructor "Float") null))
      ((struct c/Function (p b)) (match-let* ((pt (newVariable))
                                       ((list bt bc) (reconstructType (cons (make-Assumption p pt) context) b)))
                            (list (t/make-Application (t/make-Application (t/make-Function) pt) bt) bc)))
      ((struct c/If (g t e)) (match-let (((list gt gc) (reconstructType context g))
                                  ((list tt tc) (reconstructType context t))
                                  ((list et ec) (reconstructType context e)))
                        (list tt (append (list (make-Constraint gt (t/make-Constructor "Bool")) (make-Constraint tt et)) gc tc ec))))
      ((? c/Integer? _) (list (t/make-Constructor "Int") null))
      ((struct c/Let (d b)) (match-let* ((names (map (match-lambda ((struct c/Declaration (l _)) l)) d))
                                  (tyvars (map (lambda (x) (newVariable)) d))
                                  ((list types constraints)
                                   (values->list (unzip2 (map (match-lambda ((struct c/Declaration (_ r))
                                                                             (reconstructType (append (zipWith make-Assumption names tyvars) context) r))) d))))
                                  (subst (unify (append (zipWith make-Constraint tyvars types) (foldl append null constraints))))
                                  (contextS (substituteContext subst context))
                                  (typesS (map (lambda (x) (generalize contextS (substituteManyType subst x))) types)))
                       (reconstructType (append (zipWith make-Assumption names typesS) contextS) b)))
      ((struct c/ListConstructor ()) (list (t/make-Application (t/make-List) (newVariable)) null))
      ((struct c/TupleConstructor (a)) (let ((t (map (lambda (x) (newVariable)) (make-list a))))
                                  (list (foldr (lambda (x y) (t/make-Application (t/make-Application (t/make-Function) x) y))
                                               (foldl (lambda (x y) (t/make-Application y x)) (t/make-Tuple a) t) t) null)))
      ((struct c/UnitConstructor ()) (list (t/make-Unit) null))
      ((struct c/Variable (n)) (match (findf (lambda (x) (equal? (Assumption-name x) n)) context)
                          (#f (error 'reconstructType "~a is not in scope" n))
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
    (match-let (((list t c) (reconstructType primitives syntax)))
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
    (delete-duplicates (typeVariables type)))
  
  (define (wellTyped syntax) (moduleTypes primitives syntax) #t)
  
  (define primitives
    (let* ((typeParsers (parsers "TypeChecking"))
           (parseD (parser 'declaration typeParsers))
           (parseT (parser 'type typeParsers)))
      (append (list (make-Assumption "error" (generalize null (transformType (parseT "[Char] -> a"))))
                    (make-Assumption "fst" (generalize null (transformType (parseT "(a, b) -> a"))))
                    (make-Assumption "head" (generalize null (transformType (parseT "[a] -> a"))))
                    (make-Assumption "null" (generalize null (transformType (parseT "[a] -> Bool"))))
                    (make-Assumption "snd" (generalize null (transformType (parseT "(a, b) -> b"))))
                    (make-Assumption "tail" (generalize null (transformType (parseT "[a] -> [a]"))))
                    (make-Assumption ":" (generalize null (transformType (parseT "a -> [a] -> [a]")))))
              (dataContext (transformSyntax (parseD "data Bool = True | False")))))))