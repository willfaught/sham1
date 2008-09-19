(module TypeChecker mzscheme
  (require (only (lib "1.ss" "srfi") alist-cons delete-duplicates filter lset-intersection make-list partition unzip2 zip)
           (only (lib "71.ss" "srfi") values->list)
           (only (lib "list.ss") foldl foldr)
           (only (lib "match.ss") match)
           (prefix c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (lib "List.ss" "sham" "haskell")
           (lib "Parsers.ss" "sham" "haskell")
           (prefix t/ (lib "Types.ss" "sham"))
           (lib "type-operations.ss" "sham"))
  
  (provide moduleContext syntaxType)
  
  (define-struct assumption (name type) #f)
  
  (define-struct constraint (left right) #f)
  
  (define-struct substitution (old new) #f)
  
  ; contains-type? :: type type -> boolean
  (define (contains-type? container contained)
    (cond ((equal? container contained) #t)
          ((type-application? container) (or (contains-type? (type-application-operator container) contained)
                                             (contains-type? (type-application-operand container) contained)))
          (else #f)))
  
  ; module-context :: [(string, type)] module-term -> [(string, type)]
  (define (module-context ic m)
    (match-let* (((da de) (values->list (partition data-term? (module-term-declarations m))))
                 (dc (foldl append null (map data-context da)))
                 (di (map (lambda (x) (car (declaration-term-patterns x))) de))
                 (dt (map (lambda (x) (new-type-variable)) de))
                 (tc (append (zip di dt) dc ic))
                 ((t c) (lunzip2 (map (lambda (x) (reconstruct-type tc x)) de)))
                 (s (unify-constraints (append (zip-with make-constraint dt t) (foldl append null c))))
                 (st (map (lambda (x) (substitute-types s x)) t)))
      (append dc (zip di st))))
  
  ; data-context :: data-term -> [(string, type)]
  (define (data-context d)
    ; field :: type-constructor field-term -> (string type)
    (define (field dt f)
      (match-let ((($ field-term fi ft) f))
        (list fi (make-function-type dt ft))))
    ; constructor :: type-constructor constructor-term -> ((string type))
    (define (constructor dt c)
      (match-let ((($ constructor-term ci cf) c))
        (append (list (list ci (foldr make-function-type dt (map (match-lambda (($ field-term _ t) t)) cf)))
                      (list (if (equal? (string-ref ci 0) #\:)
                                (string-append ci "?")
                                (string-append "is" ci))
                            (make-function-type dt (make-type-constructor "Bool"))))
                (map (lambda (x) (field dt x)) cf))))
    (match-let ((($ data-term di dc) d))
      (foldl append null (map (lambda (x) (constructor (make-type-constructor di) x)) dc))))
  
  ; prelude :: [assumption]
  (define prelude
    (list (make-assumption "fst" (make-type-application (make-type-application (make-function-constructor)
                                                                               (make-type-application (make-type-application (make-tuple-constructor 2)
                                                                                                                             (make-type-variable "a"))
                                                                                                      (make-type-variable "b")))
                                                        (make-type-variable "a")))
          ("head" ,(make-forall-type (list (make-type-variable "a"))
                                     (make-function-type (make-list-type (make-type-variable "a"))
                                                         (make-type-variable "a"))))
          ("isFalse" ,(make-function-type (make-type-constructor "Bool")
                                          (make-type-constructor "Bool")))
          ("isTrue" ,(make-function-type (make-type-constructor "Bool")
                                         (make-type-constructor "Bool")))
          ("null" ,(make-forall-type (list (make-type-variable "a"))
                                     (make-function-type (make-list-type (make-type-variable "a"))
                                                         (make-type-constructor "Bool"))))
          
          ("snd" ,(make-forall-type (make-type-variable "a")
                                    (make-forall-type (make-type-variable "b")
                                                      (make-type-application (make-type-application (make-function-constructor)
                                                                                                    (make-type-application (make-type-application (make-tuple-constructor 2)
                                                                                                                                                  (make-type-variable "a"))
                                                                                                                           (make-type-variable "b")))
                                                                             (make-type-variable "b")))))
          ("tail" ,(make-forall-type (list (make-type-variable "a"))
                                     (make-function-type (make-list-type (make-type-variable "a"))
                                                         (make-list-type (make-type-variable "a")))))
          ("False" ,(make-type-constructor "Bool"))
          ("True" ,(make-type-constructor "Bool"))
          (":" ,(make-forall-type (list (make-type-variable "a"))
                                  (make-function-type (make-type-variable "a")
                                                      (make-function-type (make-list-type (make-type-variable "a"))
                                                                          (make-list-type (make-type-variable "a"))))))
          ("()" ,(make-type-constructor "()"))))
  
  ; syntaxType :: [assumption] c/CoreSyntax -> t/Type
  (define (syntaxType context syntax)
    (match-let (((type constraints) (reconstruct-type context term)))
      (normalize-type-variables (substitute-types (unify-constraints constraints) type))))
  
  ; reconstructType :: [assumption] c/CoreSyntax -> (t/Type, [constraint])
  (define (reconstructType context syntax)
    (match syntax
      (($ c/Application r d)
       (match-let (((rt rc) (reconstructType context r))
                   ((dt dc) (reconstructType context d))
                   (type (new-type-variable)))
         (list type (cons (make-constraint rt (t/make-Application (t/make-Application (t/make-Function) dt) type)) (append rc dc)))))
      ((? c/Character? _) (list (t/make-Constructor "Char") null))
      #;(($ declaration-term p e) (if (equal? (length p) 1)
                                      (reconstruct-type context e)
                                      (reconstruct-type context (make-function-term (cdr p) e))))
      ((? c/Float? _) (list (t/make-Constructor "Float") null))
      (($ c/Function p b)
       (match-let ((pt (new-type-variable))
                   ((bt bc) (reconstructType (append (list p pt) context) b)))
         (list (t/make-Application (t/make-Application (t/make-Function) pt) bt) bc)))
      (($ if-term g t e)
       (match-let (((gt gc) (reconstruct-type context g))
                   ((tt tc) (reconstruct-type context t))
                   ((et ec) (reconstruct-type context e)))
         (list tt (append (list (make-constraint gt (make-type-constructor "Bool")) (make-constraint tt et)) gc tc ec))))
      ((? integer-term? _)
       (list (make-type-constructor "Int") null))
      (($ let-term ds b)
       (match-let* ((ns (map (match-lambda ((n _) n)) ds))
                    (ts (map (lambda (x) (new-type-variable)) ns))
                    (dx (append (zip ns ts) context))
                    ((dt dc) (lunzip2 (map (match-lambda ((n t) (reconstruct-type dx t))) ds)))
                    (s (unify-constraints (append (zip-with make-constraint ts dt) (foldl append null dc))))
                    (context (substitute-context context s))
                    (dt (map (lambda (x) (generalize context (substitute-types s x))) dt))
                    (context (append (zip ns dt) context)))
         (reconstruct-type context b)))
      ((? list-constructor-term? _)
       (let ((t (new-type-variable)))
         (make-type-application (make-type-application (make-type-application (make-function-constructor) t)
                                                       (make-type-application (make-list-constructor) t))
                                (make-type-application (make-list-constructor) t))))
      (($ ml-term type _) (list type null))
      (($ scheme-term type _) (list type null))
      (($ tuple-constructor-term a)
       (let ((t (map (lambda (x) (new-type-variable)) (make-list a))))
         (list (foldr (lambda (x y) (make-type-application (make-type-application (make-function-constructor) x) y))
                      (foldl (lambda (x y) (make-type-application y x)) (make-tuple-constructor a) t)
                      t)
               null)))
      ((? c/Unit?) (t/make-Unit))
      (($ c/Variable n)
       (let ((type (match (assoc n prelude)
                     ((_ type) type)
                     (_ (match (assoc n context)
                          ((_ type) type)
                          (_ (error 'reconstruct-type "~a is not in the scope" n)))))))
         (if (forall-type? type)
             (list (instantiate type) null)
             (list type null))))))
  
  ; generalize :: [(string, type)] -> type -> type
  (define (generalize context type)
    ; type-in-context? :: [(string, type)] -> type -> boolean
    (define (type-in-context? context type)
      (match context
        (((_ t) . tail) (if (contains-type? type t) #t (type-in-context? tail type)))
        (() #f)))
    ; type-variables :: type -> [type-variable]
    (define (type-variables type)
      (match type
        (($ type-variable i) (list (make-type-variable i)))
        (($ function-type p r) (append (type-variables p) (type-variables r)))
        (($ list-type t) (type-variables t))
        (($ tuple-type t) (foldr append null (map type-variables t)))
        (_ null)))
    (let ((t (delete-duplicates (filter (lambda (x) (not (type-in-context? context x))) (type-variables type)))))
      (if (null? t)
          type
          (make-forall-type t type))))
  
  ; instantiate :: forall-type -> type
  (define (instantiate type)
    (match type
      (($ forall-type (head . tail) t) (let ((v (new-type-variable)))
                                         (instantiate (make-forall-type tail (map-type (lambda (x) (if (equal? head x) v x)) t)))))
      (($ forall-type () t) t)))
  
  ; mapType :: (type -> type) type -> type
  (define (mapType m t)
    (match t
      (($ t/Application r d) (m (t/make-Application (m r) (m d))))
      (_ (m t))))
  
  ; substituteConstraint :: substitution constraint -> constraint
  (define (substituteConstraint s c)
    (make-constraint (substituteType s (constraint-left c))
                     (substituteType s (constraint-right c)))))
  
  ; substituteContext :: [substitution] [assumption] -> [assumption]
  (define (substituteContext s c)
    (if (null? s) c (substituteContext (cdr s) (map (match-lambda (($ assumption n t) (make-assumption n (substituteType (car s) t)))) c))))
  
  ; substituteType :: substitution type -> type
  (define (substituteType s t)
    (mapType (lambda (x) (if (equal? x (substitution-old s)) (substitution-new s) x)) t))
  
  ; substituteManyType :: [substitution] type -> type
  (define (substituteManyType s t)
    (if (null? s) t (substituteManyType (cdr s) (substituteType (car s) t))))
  
  ; unify :: [constraint] -> [substitution]
  (define unify
    (match-lambda
      (() null)
      ((($ constraint t1 t2) . rest)
       (cond ((equal? t1 t2) (unify rest))
             ((and (t/Application? t1) (t/Application? t2))
              (unify (cons (make-constraint (t/Application-operator t1) (t/Application-operator t2))
                           (cons (make-constraint (t/Application-operand t1) (t/Application-operand t2)) rest))))
             ((and (t/Variable? t1) (not (contains-type? t2 t1)))
              (let ((s (make-substitution t1 t2))) (cons s (unify (map (lambda (x) (substitute-constraint s x)) rest)))))
             ((and (t/Variable? t2) (not (contains-type? t1 t2)))
              (let ((s (make-substitution t2 t1))) (cons s (unify (map (lambda (x) (substitute-constraint s x)) rest)))))
             (else (error 'unify "cannot unify ~a with ~a" t1 t2)))))))