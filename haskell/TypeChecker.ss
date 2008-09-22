(module TypeChecker mzscheme
  (require (only (lib "1.ss" "srfi") alist-cons delete-duplicates filter lset-intersection make-list partition unzip2 zip)
           (only (lib "71.ss" "srfi") values->list)
           (only (lib "list.ss") foldl foldr)
           (only (lib "match.ss") match match-lambda match-let match-let*)
           (prefix c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (only (lib "List.ss" "sham" "haskell") iterate zipWith)
           (only (lib "SyntaxTransformer.ss" "sham" "haskell") transformHC)
           (only (lib "Parsers.ss" "sham" "haskell") declarationParser typeParser)
           (prefix t/ (lib "Types.ss" "sham")))
  
  (provide moduleContext syntaxType)
  
  (define-struct assumption (name type) #f)
  
  (define-struct constraint (left right) #f)
  
  (define-struct substitution (old new) #f)
  
  (define variableCount 0)
  
  ; constructorContext :: t/Constructor c/Constructor -> [assumption]
  (define (constructorContext dataType syntax)
    (match-let ((($ c/Constructor n f) syntax))
      (append (list (make-assumption n (foldr (lambda (x y) (t/make-Application (t/make-Application (t/make-Function) x) y)) dataType
                                              (map (match-lambda (($ c/Field _ t) (transformHC t))) f)))
                    (make-assumption (string-append "is" n)
                                     (t/make-Application (t/make-Application (t/make-Function) dataType)
                                                         (t/make-Constructor "Bool"))))
              (map (lambda (x) (fieldContext dataType x)) f))))
  
  ; containsType :: t/Type t/Type -> boolean
  (define (containsType container contained)
    (cond ((equal? container contained) #t)
          ((t/Application? container) (or (containsType (t/Application-operator container) contained)
                                          (containsType (t/Application-operand container) contained)))
          (else #f)))
  
  ; dataContext :: c/Data -> [assumption]
  (define dataContext
    (match-lambda
      (($ c/Data n c) (foldl append null (map (lambda (x) (constructorContext (t/make-Constructor n) x)) c)))))
  
  ; fieldContext :: t/Constructor c/Field -> assumption
  (define (fieldContext dataType syntax)
    (match-let ((($ c/Field n t) syntax))
      (make-assumption n (t/make-Application (t/make-Application (t/make-Function) dataType) (transformHC t)))))
  
  ; instantiate :: t/Type -> t/Type
  (define (instantiate type)
    (let ((vars (uniqueTypeVariables type)))
      (rename (zip vars (map (lambda (x) (newVariable)) vars)) type)))
  
  ; mapType :: (t/Type -> t/Type) t/Type -> t/Type
  (define (mapType mapper type)
    (match type
      (($ t/Application r d) (mapper (t/make-Application (mapper r) (mapper d))))
      (_ (mapper type))))
  
  ; moduleContext :: [assumption] c/Module -> [assumption]
  (define (moduleContext context syntax)
    (match-let* (((data decl) (values->list (partition c/Data? (c/Module-declarations syntax))))
                 (declNames (map (match-lambda (($ c/Declaration n _) n)) decl))
                 (declTyvars (map (lambda (x) (newVariable)) decl))
                 (declContext (append (zip declNames declTyvars) (foldl append null (map dataContext data)) context))
                 ((declTypes declConstraints) (values->list (unzip2 (map (lambda (x) (reconstructType declContext x)) decl))))
                 (subst (unify (append (zipWith make-constraint declTyvars declTypes) (foldl append null declConstraints))))
                 (fullDeclTypes (map (lambda (x) (substituteManyType subst x)) declTypes)))
      (append (zip declNames fullDeclTypes) context)))
  
  ; newVariable :: t/Variable
  (define (newVariable)
    (set! variableCount (+ variableCount 1))
    (t/make-Variable (string-append "t" (number->string variableCount))))
  
  ; normalize :: t/Type -> t/Type
  (define (normalize type)
    (let ((vars (uniqueTypeVariables type)))
      (rename (zip vars (map (lambda (x) (t/make-Variable (if (equal? x 0) "t" (string-append "t" (number->string x)))))
                             (iterate (lambda (x) (+ x 1)) 0 (length vars)))) type)))
  
  ; primitives :: [assumption]
  (define primitives
    (let ((parseD (declarationParser "primitives"))
          (parseT (typeParser "primitives")))
      (append (list (make-assumption "fst" (transformHC (parseT "(a, b) -> a")))
                    (make-assumption "head" (transformHC (parseT "[a] -> a")))
                    (make-assumption "snd" (transformHC (parseT "(a, b) -> b")))
                    (make-assumption "tail" (transformHC (parseT "[a] -> [a]")))
                    (make-assumption ":" (transformHC (parseT "a -> [a] -> [a]"))))
              (dataContext (transformHC (parseD "data Bool = True | False"))))))
  
  ; reconstructType :: [assumption] c/CoreSyntax -> (t/Type, [constraint])
  (define (reconstructType context syntax)
    (match syntax
      (($ c/Application r d) (match-let (((rt rc) (reconstructType context r))
                                         ((dt dc) (reconstructType context d))
                                         (t (newVariable)))
                               (list t (cons (make-constraint rt (t/make-Application (t/make-Application (t/make-Function) dt) t)) (append rc dc)))))
      ((? c/Character? _) (list (t/make-Constructor "Char") null))
      ((? c/Float? _) (list (t/make-Constructor "Float") null))
      (($ c/Function p b) (match-let* ((pt (newVariable))
                                       ((bt bc) (reconstructType (append (list p pt) context) b)))
                            (list (t/make-Application (t/make-Application (t/make-Function) pt) bt) bc)))
      (($ c/If g t e) (match-let (((gt gc) (reconstructType context g))
                                  ((tt tc) (reconstructType context t))
                                  ((et ec) (reconstructType context e)))
                        (list tt (append (list (make-constraint gt (t/make-Constructor "Bool")) (make-constraint tt et)) gc tc ec))))
      ((? c/Integer? _) (list (t/make-Constructor "Int") null))
      (($ c/Let d b) (match-let* ((names (map (match-lambda (($ c/Declaration l _) l)) d))
                                  (tyvars (map (lambda (x) (newVariable)) d))
                                  ((types constraints) (values->list (unzip2 (map (match-lambda (($ c/Declaration _ r)
                                                                                                 (reconstructType (append (zip names tyvars) context) r))) d))))
                                  (subst (unify (append (zipWith make-constraint tyvars types) (foldl append null constraints))))
                                  (contextS (substituteContext subst context))
                                  (typesS (map (lambda (x) (substituteManyType subst x)) types)))
                       (reconstructType (append (zip names typesS) contextS) b)))
      (($ c/ListConstructor) (list (t/make-Application (t/make-List) (newVariable)) null))
      (($ c/ML t _) (list (transformHC t) null))
      (($ c/Scheme t _) (list (transformHC t) null))
      (($ c/TupleConstructor a) (let ((t (map (lambda (x) (newVariable)) (make-list a))))
                                  (list (foldr (lambda (x y) (t/make-Application (t/make-Application (t/make-Function) x) y))
                                               (foldl (lambda (x y) (t/make-Application y x)) (t/make-Tuple a) t) t) null)))
      (($ c/UnitConstructor) (t/make-Unit))
      (($ c/Variable n) (match (assoc n context)
                          ((_ t) t)
                          (_ (error 'reconstructType "~a is not in scope" n))))))
  
  ; rename :: [(t/Variable, t/Variable)] t/Type -> t/Type
  (define (rename mappings type)
    (mapType (match-lambda ((? t/Variable? x) (match (assoc x mappings) ((_ y) y) (#f x))) (x x)) type))
  
  ; substituteConstraint :: substitution constraint -> constraint
  (define (substituteConstraint s c)
    (make-constraint (substituteType s (constraint-left c))
                     (substituteType s (constraint-right c))))
  
  ; substituteContext :: [substitution] [assumption] -> [assumption]
  (define (substituteContext s c)
    (if (null? s) c (substituteContext (cdr s) (map (match-lambda (($ assumption n t) (make-assumption n (substituteType (car s) t)))) c))))
  
  ; substituteType :: substitution type -> type
  (define (substituteType s t)
    (mapType (lambda (x) (if (equal? x (substitution-old s)) (substitution-new s) x)) t))
  
  ; substituteManyType :: [substitution] type -> type
  (define (substituteManyType s t)
    (if (null? s) t (substituteManyType (cdr s) (substituteType (car s) t))))
  
  ; syntaxType :: [assumption] c/CoreSyntax -> t/Type
  (define (syntaxType context syntax)
    (match-let (((t c) (reconstructType context syntax)))
      (normalize (substituteManyType (unify c) t))))
  
  ; typeVariables :: t/Type -> [t/Variable]
  (define typeVariables
    (match-lambda
      (($ t/Application r d) (append (typeVariables r) (typeVariables d)))
      ((? t/Variable? x) (list x))
      (_ null)))
  
  ; unify :: [constraint] -> [substitution]
  (define unify
    (match-lambda
      (() null)
      ((($ constraint t1 t2) . rest)
       (cond ((equal? t1 t2) (unify rest))
             ((and (t/Application? t1) (t/Application? t2))
              (unify (cons (make-constraint (t/Application-operator t1) (t/Application-operator t2))
                           (cons (make-constraint (t/Application-operand t1) (t/Application-operand t2)) rest))))
             ((and (t/Variable? t1) (not (containsType t2 t1)))
              (let ((s (make-substitution t1 t2))) (cons s (unify (map (lambda (x) (substituteConstraint s x)) rest)))))
             ((and (t/Variable? t2) (not (containsType t1 t2)))
              (let ((s (make-substitution t2 t1))) (cons s (unify (map (lambda (x) (substituteConstraint s x)) rest)))))
             (else (error 'unify "cannot unify ~a with ~a" t1 t2))))))
  
  ; uniqueTypeVariables :: t/Type -> [t/Variable]
  (define (uniqueTypeVariables type)
    (delete-duplicates (typeVariables type))))