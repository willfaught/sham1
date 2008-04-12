(module haskell-typechecker mzscheme
  (require (only (lib "1.ss" "srfi") make-list unzip2 zip)
           (lib "haskell-compiler.ss" "hs")
           (lib "haskell-prelude.ss" "hs")
           (lib "haskell-terms.ss" "hs")
           (lib "haskell-types.ss" "hs")
           (only (lib "list.ss") foldl)
           (lib "match.ss")
           (lib "test.ss" "hs"))
  
  (provide valid-types)
  
  (define (valid-types module)
    (unify-constraints (reconstruct-module-types module))
    #t)
  
  (define type-variable-count 0)
  
  (define (fresh-type-variable)
    (set! type-variable-count (+ type-variable-count 1))
    (make-type-variable (string-append "t" (number->string type-variable-count))))
  
  (define-struct constraint (left right) #f)
  
  (define-struct type-mapping (from-type to-type) #f)
  
  (define (lunzip2 x)
    (let-values (((x y) (unzip2 x))) (list x y)))
  
  (define (zip-with f x y)
    (if (equal? (length x) (length y))
        (if (null? x) null (cons (f (car x) (car y)) (zip-with f (cdr x) (cdr y))))
        (error 'zip-with "lists have different lengths")))
  
  (define (translate-type type)
    (match type
      (($ type-constructor "Bool") (make-boolean-type))
      (($ type-constructor "Char") (make-character-type))
      (($ type-constructor "Int") (make-integer-type))
      (($ type-constructor "Integer") (make-integer-type))
      (($ type-constructor "Float") (make-float-type))
      (($ function-type t) (make-function-type (map translate-type t)))
      (x x)))
  
  (define (reconstruct-types context term)
    (match term
      (($ application-term f a) (match-let* (((f-type f-constraints) (reconstruct-types context f))
                                             ((a-types a-constraints) (lunzip2 (map (lambda (x) (reconstruct-types context x)) a)))
                                             (type (fresh-type-variable))
                                             (constraints (cons (make-constraint f-type (make-function-type (append a-types (list type))))
                                                                (append f-constraints (foldl append null a-constraints)))))
                                  (list type constraints)))
      #;(($ case-term e a) (match-let* (((e-type e-constraints) (reconstruct-types context e)) ; need to add constraints between e and patterns
                                        ((a-types a-constraints) (lunzip2 (map (lambda (x) (reconstruct-types (cons (list (car x) e-type) context) (cdr x))) a))))
                             (list (car a-types) (append e-constraints (foldl append null a-constraints)))))
      (($ character-term c) (list (make-character-type) null))
      (($ declaration-term p e t) (if (equal? (length p) 1)
                                      (match-let (((type constraints) (reconstruct-types context e)))
                                        (list type (cons (make-constraint (list-ref (assoc (car p) context) 1) type) constraints)))
                                      (match-let (((type constraints) (reconstruct-types context (make-function-term (cdr p) e t))))
                                        (list type (cons (make-constraint (list-ref (assoc (car p) context) 1) type) constraints)))))
      (($ float-term f) (list (make-float-type) null))
      (($ function-term p b _) (match-let* ((p-types (map (lambda (x) (fresh-type-variable)) p)) 
                                            ((type constraints) (reconstruct-types (append (zip p p-types) context) b)))
                                 (list (make-function-type (append p-types (list type))) constraints)))
      (($ identifier-term i) (match (assoc i context)
                               ((_ type) (list type null))
                               (_ (error 'reconstruct-types "Not in scope: '~a'" i))))
      (($ if-term g t e) (match-let (((g-type g-constraints) (reconstruct-types context g))
                                     ((t-type t-constraints) (reconstruct-types context t))
                                     ((e-type e-constraints) (reconstruct-types context e)))
                           (list t-type (append g-constraints t-constraints e-constraints (list (make-constraint g-type (make-boolean-type))
                                                                                                (make-constraint t-type e-type))))))
      (($ integer-term i) (list (make-integer-type) null))
      (($ let-term d e) (match-let* ((context-2 (append (map (lambda (x) (list (car (declaration-term-patterns x)) (fresh-type-variable))) d) context))
                                     ((d-types d-constraints) (lunzip2 (map (lambda (x) (reconstruct-types context-2 x)) d)))
                                     ((e-type e-constraints) (reconstruct-types context-2 e)))
                          (list e-type (append (foldl append null d-constraints) e-constraints))))
      (($ list-term e) (if (null? e)
                           (list (make-list-type (fresh-type-variable)) null)
                           (match-let ((((head-type . tail-types) e-constraints) (lunzip2 (map (lambda (x) (reconstruct-types context x)) e))))
                             (list (make-list-type head-type)
                                   (append (map (lambda (x) (make-constraint head-type x)) tail-types)
                                           (foldl append null e-constraints))))))
      (($ tuple-term e) (match-let (((e-types e-constraints) (lunzip2 (map (lambda (x) (reconstruct-types context x)) e))))
                          (list (make-tuple-type e-types) (foldl append null e-constraints))))
      (($ tuplecon-term a) (let ((types (map (lambda (x) (fresh-type-variable)) (make-list a))))
                             (list (make-function-type (append types (list (make-tuple-type types)))) null)))
      ))
  
  (define rt reconstruct-types)
  
  (define (reconstruct-module-types module)
    (match-let* ((decls (module-term-declarations module))
                 (context (map (lambda (x) (list (car (declaration-term-patterns x)) (fresh-type-variable))) decls))
                 ((types constraints) (lunzip2 (map (lambda (x) (reconstruct-types context x)) decls))))
      (foldl append null constraints)))
  
  (define rmt reconstruct-module-types)
  
  (define (contains-type? container-type containee-type)
    (if (equal? container-type containee-type)
        #t
        (match container-type
          (($ function-type t) (foldl (lambda (x y) (or x y)) #f (map (lambda (x) (contains-type? x containee-type)) t)))
          (($ list-type t) (contains-type? t containee-type))
          (($ tuple-type t) (foldl (lambda (x y) (or x y)) #f (map (lambda (x) (contains-type? x containee-type)) t)))
          (_ #f))))
  
  (define (substitute-in-constraints from-type to-type constraints)
    (map (match-lambda (($ constraint left-type right-type) (make-constraint (substitute-in-type (list (list from-type to-type)) left-type)
                                                                             (substitute-in-type (list (list from-type to-type)) right-type)))) constraints))
  
  (define (substitute-in-type mappings from-type)
    (match (assoc from-type mappings)
      ((_ to-type) to-type)
      (#f (match from-type
            (($ function-type t) (make-function-type (map (lambda (x) (substitute-in-type mappings x)) t)))
            (($ list-type t) (make-list-type (substitute-in-type mappings t)))
            (($ tuple-type t) (make-tuple-type (map (lambda (x) (substitute-in-type mappings x)) t)))
            (t t)))))
  
  (define (unify-constraints constraints)
    (define (zip-function-types types)
      (match types
        (((a . ()) (b c . d)) (list (make-constraint a (make-function-type (append (list b c) d)))))
        (((a b . c) (d . ())) (list (make-constraint (make-function-type (append (list a b) c)) d)))
        (((a . b) (c . d)) (cons (make-constraint a c) (zip-function-types (list b d))))
        ((() ()) null)))
    (match constraints
      ((($ constraint left-type right-type) . rest)
       (cond ((equal? left-type right-type)
              (unify-constraints rest))
             ((and (type-variable? left-type)
                   (not (contains-type? right-type left-type)))
              (cons (list left-type right-type)
                    (unify-constraints (substitute-in-constraints left-type right-type rest))))
             ((and (type-variable? right-type)
                   (not (contains-type? left-type right-type)))
              (cons (list right-type left-type)
                    (unify-constraints (substitute-in-constraints right-type left-type rest))))
             ((and (function-type? left-type)
                   (function-type? right-type))
              (unify-constraints (append (zip-function-types (list (function-type-types left-type) (function-type-types right-type))) rest)))
             (else (error 'unify-constraints "cannot unify the constraint: ~a = ~a" left-type right-type))))
      (() null)))
  
  (define uc unify-constraints)
  
  (define tests
    (list (make-test "character-term 1"
                     (make-character-term "a")
                     (make-character-type))
          (make-test "integer-term 1"
                     (make-integer-term "1")
                     (make-integer-type))
          (make-test "float-term 1"
                     (make-float-term "2.3")
                     (make-float-type))
          (make-test "list-term 1"
                     (make-list-term null)
                     (make-list-type (make-type-variable "t1")))
          (make-test "list-term 2"
                     (make-list-term (list (make-character-term "a")))
                     (make-list-type (make-character-type)))
          (make-test "list-term 3"
                     (make-list-term (list (make-float-term "1.2") (make-float-term "3.4")))
                     (make-list-type (make-float-type)))
          (make-test "tuple-term 1"
                     (make-tuple-term (list (make-character-term "a") (make-float-term "1.2")))
                     (make-tuple-type (list (make-character-type) (make-float-type))))
          (make-test "tuple-term 2"
                     (make-tuple-term (list (make-character-term "a") (make-float-term "1.2") (make-integer-term "3")))
                     (make-tuple-type (list (make-character-type) (make-float-type) (make-integer-type))))
          (make-test "application-term 1"
                     (make-application-term (make-function-term (list "x") (make-integer-term "1") #f)
                                            (list (make-integer-term "1")))
                     (make-integer-type))
          (make-test "application-term 2"
                     (make-application-term (make-function-term (list "x") (make-identifier-term "x") #f)
                                            (list (make-integer-term "1")))
                     (make-integer-type))
          ))
  
  (define (run-all-tests)
    (run-tests (lambda (x)
                 (set! type-variable-count 0)
                 (match-let* (((type constraints) (reconstruct-types null x)))
                   (substitute-in-type (unify-constraints constraints) type)))
               tests)))