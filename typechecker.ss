; known issues:
;   - there cannot be ambiguity for an identifier occurrence; the sets of exported module identifiers must be disjoint

(module typechecker mzscheme
  (require (only (lib "1.ss" "srfi") alist-cons delete-duplicates filter lset-intersection make-list unzip2 zip)
           (lib "compiler.ss" "haskell")
           (lib "list.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "types.ss" "haskell")
           (only (lib "list.ss") foldl foldr)
           (lib "match.ss")
           (lib "reader.ss" "haskell"))
  
  (provide module-context reconstruct-type)
  
  (define-struct constraint (left-type right-type) #f)
  
  ; module-context :: [(string, type)] module-term -> [(string, type)]
  (define (module-context import-context module)
    (match-let* ((declarations (module-term-declarations module))
                 (identifiers (map (lambda (x) (car (declaration-term-patterns x))) declarations))
                 (context (append (map (lambda (x) (list x (fresh-type-variable))) identifiers) import-context))
                 ((types constraints) (lunzip2 (map (lambda (x) (reconstruct-types context x)) declarations)))
                 (substitution (unify-constraints (foldl append null constraints)))
                 (substituted-types (map (lambda (x) (substitute-types substitution x)) types)))
      (zip identifiers substituted-types)))
  
  ; reconstruct-type :: [(string, type)] term -> type
  (define (reconstruct-type context term)
    (match-let (((type constraints) (reconstruct-types context term)))
      (normalize-type-variables (substitute-types (unify-constraints constraints) type))))
  
  ; reconstruct-types :: [(string, type)] term -> (type, [constraint])
  (define (reconstruct-types context term)
    (match term
      (($ application-term f a) (match-let* (((f-type f-constraints) (reconstruct-types context f))
                                             ((a-types a-constraints) (lunzip2 (map (lambda (x) (reconstruct-types context x)) a)))
                                             (type (fresh-type-variable))
                                             (constraints (cons (make-constraint f-type (foldr (lambda (x y) (make-function-type x y)) type a-types))
                                                                (append f-constraints (foldl append null a-constraints)))))
                                  (list type constraints)))
      #;(($ case-term e a) )
      (($ character-term c) (list (make-character-type) null))
      (($ declaration-term p e) (if (equal? (length p) 1)
                                    (reconstruct-types context e)
                                    (reconstruct-types context (make-function-term (cdr p) e))))
      (($ float-term f) (list (make-float-type) null))
      (($ function-term p b) (match-let* ((p-types (map (lambda (x) (fresh-type-variable)) p)) 
                                          ((type constraints) (reconstruct-types (append (zip p p-types) context) b)))
                               (list (foldr (lambda (x y) (make-function-type x y)) type p-types) constraints)))
      (($ identifier-term i) (match (assoc i context)
                               ((_ type) (if (universal-type? type)
                                             (list (instantiate type) null)
                                             (list type null)))
                               (_ (if (equal? i ":")
                                      (let ((t (fresh-type-variable)))
                                        (list (make-function-type t (make-function-type (make-list-type t) (make-list-type t))) null))
                                      (error 'reconstruct-types "Not in scope: '~a'" i)))))
      (($ if-term g t e) (match-let (((g-type g-constraints) (reconstruct-types context g))
                                     ((t-type t-constraints) (reconstruct-types context t))
                                     ((e-type e-constraints) (reconstruct-types context e)))
                           (list t-type (append g-constraints t-constraints e-constraints (list (make-constraint g-type (make-boolean-type))
                                                                                                (make-constraint t-type e-type))))))
      (($ integer-term i) (list (make-integer-type) null))
      (($ let-term d e) (match-let* ((identifiers (map (match-lambda (($ declaration-term p _) (car p))) d))
                                     (type-variables (map (lambda (x) (fresh-type-variable)) identifiers))
                                     (context-2 (append (zip identifiers type-variables) context))
                                     ((d-types d-constraints) (lunzip2 (map (lambda (x) (reconstruct-types context-2 x)) d)))
                                     (substitution (unify-constraints (append (zip-with make-constraint type-variables d-types) (foldl append null d-constraints))))
                                     (context (substitute-in-context context substitution))
                                     (d-types (map (lambda (x) (generalize context (substitute-types substitution x))) d-types))
                                     (context (append (zip identifiers d-types) context)))
                          (reconstruct-types context e)))
      (($ list-term e) (if (null? e)
                           (list (make-list-type (fresh-type-variable)) null)
                           (match-let ((((head-type . tail-types) e-constraints) (lunzip2 (map (lambda (x) (reconstruct-types context x)) e))))
                             (list (make-list-type head-type)
                                   (append (map (lambda (x) (make-constraint head-type x)) tail-types)
                                           (foldl append null e-constraints))))))
      (($ module-term _ d) (match-let* ((identifiers (map (lambda (x) (car (declaration-term-patterns x))) d))
                                        (context (map (lambda (x) (list x (fresh-type-variable))) identifiers))
                                        ((types constraints) (lunzip2 (map (lambda (x) (reconstruct-types context x)) d))))
                             (list types (foldl append null constraints))))
      (($ scheme-term type _) (list type null))
      (($ tuple-term e) (match-let (((e-types e-constraints) (lunzip2 (map (lambda (x) (reconstruct-types context x)) e))))
                          (list (make-tuple-type e-types) (foldl append null e-constraints))))
      (($ tuplecon-term a) (let ((types (map (lambda (x) (fresh-type-variable)) (make-list a))))
                             (list (foldr (lambda (x y) (make-function-type x y)) (make-tuple-type types) types) null)))))
  
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
          (make-universal-type t type))))
  
  ; instantiate :: universal-type -> type
  (define (instantiate type)
    (match type
      (($ universal-type (head . tail) t) (let ((v (fresh-type-variable)))
                                            (instantiate (make-universal-type tail (map-type (lambda (x) (if (equal? head x) v x)) t)))))
      (($ universal-type () t) t)))
  
  ; contains-type? :: type -> type -> boolean
  (define (contains-type? container-type containee-type)
    (if (equal? container-type containee-type)
        #t
        (match container-type
          (($ function-type p r) (or (contains-type? p containee-type) (contains-type? r containee-type)))
          (($ list-type t) (contains-type? t containee-type))
          (($ tuple-type t) (foldl (lambda (x y) (or x y)) #f (map (lambda (x) (contains-type? x containee-type)) t)))
          (_ #f))))
  
  ; substitute-in-context :: [(string, type)] -> [(type, type)] -> [(string, type)]
  (define (substitute-in-context context substitution)
    (match substitution
      (((from-type to-type) . rest) (substitute-in-context (map (match-lambda ((identifier type) (list identifier (substitute-type from-type to-type type)))) context) rest))
      (() context)))
  
  ; substitute-in-constraints :: type -> type -> [constraint] -> [constraint]
  (define (substitute-in-constraints from-type to-type constraints)
    (map (match-lambda (($ constraint left-type right-type) (make-constraint (substitute-type from-type to-type left-type)
                                                                             (substitute-type from-type to-type right-type))))
         constraints))
  
  ; substitute-type :: type -> type -> type -> type
  (define (substitute-type from-type to-type type)
    (map-type (lambda (x) (if (equal? from-type x) to-type x)) type)) 
  
  ; substitute-types :: [(type, type)] -> type -> type
  (define (substitute-types mappings type)
    (match mappings
      (((from-type to-type) . tail) (substitute-types tail (substitute-type from-type to-type type)))
      (() type)))
  
  ; unify-constraints :: [constraint] -> [(type, type)]
  (define (unify-constraints constraints)
    (match constraints
      ((($ constraint t1 t2) . rest)
       (cond ((equal? t1 t2)
              (unify-constraints rest))
             ((and (type-variable? t1)
                   (not (contains-type? t2 t1)))
              (cons (list t1 t2)
                    (unify-constraints (substitute-in-constraints t1 t2 rest))))
             ((and (type-variable? t2)
                   (not (contains-type? t1 t2)))
              (cons (list t2 t1)
                    (unify-constraints (substitute-in-constraints t2 t1 rest))))
             ((and (function-type? t1)
                   (function-type? t2))
              (let ((parameters-constraint (make-constraint (function-type-parameter-type t1) (function-type-parameter-type t2)))
                    (results-constraint (make-constraint (function-type-result-type t1) (function-type-result-type t2))))
                (unify-constraints (cons parameters-constraint (cons results-constraint rest)))))
             ((and (list-type? t1)
                   (list-type? t2))
              (unify-constraints (cons (make-constraint (list-type-type t1) (list-type-type t2)) rest)))
             ((and (tuple-type? t1)
                   (tuple-type? t2)
                   (equal? (length (tuple-type-types t1)) (length (tuple-type-types t2))))
              (unify-constraints (append (zip-with make-constraint (tuple-type-types t1) (tuple-type-types t2)) rest)))
             (else (error 'unify-constraints "cannot unify the constraint: ~a = ~a" t1 t2))))
      (() null))))