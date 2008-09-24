(module TypeChecker mzscheme
  (require (only (lib "1.ss" "srfi") alist-cons delete-duplicates filter lset-intersection make-list partition unzip2 zip)
           (only (lib "71.ss" "srfi") values->list)
           (only (lib "list.ss") findf foldl foldr)
           (only (lib "match.ss") match match-lambda match-let match-let*)
           (prefix c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (only (lib "List.ss" "sham" "haskell") iterate zipWith)
           (only (lib "SyntaxTransformer.ss" "sham" "haskell") transformHC)
           (only (lib "Parsers.ss" "sham" "haskell") declarationParser typeParser)
           (prefix t/ (lib "Types.ss" "sham")))
  
  (provide moduleContext syntaxType)
  
  (define-struct Assumption (name type) #f)
  
  (define-struct Constraint (left right) #f)
  
  (define-struct (Forall t/Type) (variables type) #f)
  
  (define-struct Substitution (old new) #f)
  
  (define variableCount 0)
  
  ; constructorContext :: t/Constructor c/Constructor -> [Assumption]
  (define (constructorContext dataType syntax)
    (match-let ((($ c/Constructor n f) syntax))
      (append (list (make-Assumption n (foldr (lambda (x y) (t/make-Application (t/make-Application (t/make-Function) x) y)) dataType
                                              (map (match-lambda (($ c/Field _ t) t)) f)))
                    (make-Assumption (string-append "is" n)
                                     (t/make-Application (t/make-Application (t/make-Function) dataType)
                                                         (t/make-Constructor "Bool"))))
              (map (lambda (x) (fieldContext dataType x)) f))))
  
  ; containsType :: t/Type t/Type -> boolean
  (define (containsType container contained)
    (cond ((equal? container contained) #t)
          ((t/Application? container) (or (containsType (t/Application-operator container) contained)
                                          (containsType (t/Application-operand container) contained)))
          (else #f)))
  
  ; dataContext :: c/Data -> [Assumption]
  (define dataContext
    (match-lambda
      (($ c/Data n c) (foldl append null (map (lambda (x) (constructorContext (t/make-Constructor n) x)) c)))))
  
  ; fieldContext :: t/Constructor c/Field -> Assumption
  (define (fieldContext dataType syntax)
    (match-let ((($ c/Field n t) syntax))
      (make-Assumption n (t/make-Application (t/make-Application (t/make-Function) dataType) t))))
  
  ; generalize :: [Assumption] t/Type -> t/Type
  (define (generalize context type)
    (let ((tyvars (uniqueTypeVariables type))
          (types (map (match-lambda (($ Assumption _ t) t)) context)))
      (if (null? tyvars) type (make-Forall (map (match-lambda ((x _) x))
                                                (filter (lambda (x) (equal? (list-ref x 1) #f))
                                                        (map (lambda (x) (list x (findf (lambda (y) (containsType y x)) types))) tyvars))) type))))
  
  ; instantiate :: t/Type -> t/Type
  (define instantiate
    (match-lambda (($ Forall v t) (rename (zip v (map (lambda (x) (newVariable)) v)) t))))
  
  ; mapType :: (t/Type -> t/Type) t/Type -> t/Type
  (define (mapType mapper type)
    (match type
      (($ t/Application r d) (mapper (t/make-Application (mapType mapper r) (mapType mapper d))))
      (x (mapper x))))
  
  ; moduleContext :: [(string, t/Type)] c/Module -> [(string, t/Type)]
  (define (moduleContext context syntax)
    (match-let* ((assumCxt (map (match-lambda ((x y) (make-Assumption x y))) context))
                 ((data decl) (values->list (partition c/Data? (c/Module-declarations syntax))))
                 (names (map (match-lambda (($ c/Declaration n _) n)) decl))
                 (tyvars (map (lambda (x) (newVariable)) decl))
                 (dataCxt (foldl append null (map dataContext data)))
                 (declCxt (append (zipWith make-Assumption names tyvars) dataCxt assumCxt primitives))
                 ((types constraints) (values->list (unzip2 (map (lambda (x) (reconstructType declCxt x)) decl))))
                 (subst (unify (append (zipWith make-Constraint tyvars types) (foldl append null constraints))))
                 (typesS (map (lambda (x) (substituteManyType subst x)) types)))
      (append (zip names typesS) (map (match-lambda (($ Assumption x y) (list x y))) (append dataCxt assumCxt primitives)))))
  
  ; newVariable :: t/Variable
  (define (newVariable)
    (set! variableCount (+ variableCount 1))
    (t/make-Variable (string-append "t" (number->string variableCount))))
  
  ; normalize :: t/Type -> t/Type
  (define (normalize type)
    (let ((vars (uniqueTypeVariables type)))
      (rename (zip vars (map (lambda (x) (t/make-Variable (if (equal? x 0) "t" (string-append "t" (number->string x)))))
                             (iterate (lambda (x) (+ x 1)) 0 (length vars)))) type)))
  
  ; primitives :: [Assumption]
  (define primitives
    (let ((parseD (declarationParser "primitives"))
          (parseT (typeParser "primitives")))
      (append (list (make-Assumption "fst" (transformHC (parseT "(a, b) -> a")))
                    (make-Assumption "head" (transformHC (parseT "[a] -> a")))
                    (make-Assumption "null" (transformHC (parseT "[a] -> Bool")))
                    (make-Assumption "snd" (transformHC (parseT "(a, b) -> b")))
                    (make-Assumption "tail" (transformHC (parseT "[a] -> [a]")))
                    (make-Assumption ":" (transformHC (parseT "a -> [a] -> [a]"))))
              (dataContext (transformHC (parseD "data Bool = True | False"))))))
  
  ; reconstructType :: [Assumption] c/CoreSyntax -> (t/Type, [Constraint])
  (define (reconstructType context syntax)
    (match syntax
      (($ c/Application r d) (match-let (((rt rc) (reconstructType context r))
                                         ((dt dc) (reconstructType context d))
                                         (t (newVariable)))
                               (list t (cons (make-Constraint rt (t/make-Application (t/make-Application (t/make-Function) dt) t)) (append rc dc)))))
      ((? c/Character? _) (list (t/make-Constructor "Char") null))
      (($ c/Declaration _ r) (reconstructType context r))
      ((? c/Float? _) (list (t/make-Constructor "Float") null))
      (($ c/Function p b) (match-let* ((pt (newVariable))
                                       ((bt bc) (reconstructType (cons (make-Assumption p pt) context) b)))
                            (list (t/make-Application (t/make-Application (t/make-Function) pt) bt) bc)))
      (($ c/If g t e) (match-let (((gt gc) (reconstructType context g))
                                  ((tt tc) (reconstructType context t))
                                  ((et ec) (reconstructType context e)))
                        (list tt (append (list (make-Constraint gt (t/make-Constructor "Bool")) (make-Constraint tt et)) gc tc ec))))
      ((? c/Integer? _) (list (t/make-Constructor "Int") null))
      (($ c/Let d b) (match-let* ((names (map (match-lambda (($ c/Declaration l _) l)) d))
                                  (tyvars (map (lambda (x) (newVariable)) d))
                                  ((types Constraints)
                                   (values->list (unzip2 (map (match-lambda (($ c/Declaration _ r)
                                                                             (reconstructType (append (zipWith make-Assumption names tyvars) context) r))) d))))
                                  (subst (unify (append (zipWith make-Constraint tyvars types) (foldl append null Constraints))))
                                  (contextS (substituteContext subst context))
                                  (typesS (map (lambda (x) (generalize contextS (substituteManyType subst x))) types)))
                       (reconstructType (append (zipWith make-Assumption names typesS) contextS) b)))
      (($ c/ListConstructor) (list (t/make-Application (t/make-List) (newVariable)) null))
      (($ c/ML t _) (list (let ((t (generalize context (transformHC t)))) (if (Forall? t) (instantiate t) t)) null))
      (($ c/Scheme t _) (list (let ((t (generalize context (transformHC t)))) (if (Forall? t) (instantiate t) t)) null))
      (($ c/TupleConstructor a) (let ((t (map (lambda (x) (newVariable)) (make-list a))))
                                  (list (foldr (lambda (x y) (t/make-Application (t/make-Application (t/make-Function) x) y))
                                               (foldl (lambda (x y) (t/make-Application y x)) (t/make-Tuple a) t) t) null)))
      (($ c/UnitConstructor) (list (t/make-Unit) null))
      (($ c/Variable n) (match (findf (lambda (x) (equal? (Assumption-name x) n)) context)
                          (#f (error 'reconstructType "~a is not in scope" n))
                          (x (list (let ((t (Assumption-type x))) (if (Forall? t) (instantiate t) t)) null))))))
  
  ; rename :: [(t/Variable, t/Variable)] t/Type -> t/Type
  (define (rename mappings type)
    (mapType (match-lambda ((? t/Variable? x) (match (assoc x mappings) ((_ y) y) (#f x))) (x x)) type))
  
  ; substituteConstraint :: Substitution Constraint -> Constraint
  (define (substituteConstraint s c)
    (make-Constraint (substituteType s (Constraint-left c))
                     (substituteType s (Constraint-right c))))
  
  ; substituteContext :: [Substitution] [Assumption] -> [Assumption]
  (define (substituteContext s c)
    (if (null? s) c (substituteContext (cdr s) (map (match-lambda (($ Assumption n t) (make-Assumption n (substituteType (car s) t)))) c))))
  
  ; substituteType :: Substitution type -> type
  (define (substituteType s t)
    (mapType (lambda (x) (if (equal? x (Substitution-old s)) (Substitution-new s) x)) t))
  
  ; substituteManyType :: [Substitution] type -> type
  (define (substituteManyType s t)
    (if (null? s) t (substituteManyType (cdr s) (substituteType (car s) t))))
  
  ; syntaxType :: [(string, t/Type)] c/CoreSyntax -> t/Type
  (define (syntaxType context syntax)
    (match-let (((t c) (reconstructType (append (map (match-lambda ((x y) (make-Assumption x y))) context) primitives) syntax)))
      (normalize (substituteManyType (unify c) t))))
  
  ; typeVariables :: t/Type -> [t/Variable]
  (define typeVariables
    (match-lambda
      (($ t/Application r d) (append (typeVariables r) (typeVariables d)))
      ((? t/Variable? x) (list x))
      (_ null)))
  
  ; unify :: [Constraint] -> [Substitution]
  (define unify
    (match-lambda
      (() null)
      ((($ Constraint t1 t2) . rest)
       (cond ((equal? t1 t2) (unify rest))
             ((and (t/Application? t1) (t/Application? t2))
              (unify (cons (make-Constraint (t/Application-operator t1) (t/Application-operator t2))
                           (cons (make-Constraint (t/Application-operand t1) (t/Application-operand t2)) rest))))
             ((and (t/Variable? t1) (not (containsType t2 t1)))
              (let ((s (make-Substitution t1 t2))) (cons s (unify (map (lambda (x) (substituteConstraint s x)) rest)))))
             ((and (t/Variable? t2) (not (containsType t1 t2)))
              (let ((s (make-Substitution t2 t1))) (cons s (unify (map (lambda (x) (substituteConstraint s x)) rest)))))
             (else (error 'unify "cannot unify ~a with ~a" t1 t2))))))
  
  ; uniqueTypeVariables :: t/Type -> [t/Variable]
  (define (uniqueTypeVariables type)
    (delete-duplicates (typeVariables type))))