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
  
  (define (substitute-constraints-type constraints from-type to-type)
    (define (substitute-type from-type to-type type)
      (if (equal? from-type type)
          to-type
          (match type
            (($ function-type t) (make-function-type (map (lambda (x) (substitute-type from-type to-type x)) t)))
            (($ list-type t) (make-list-type (substitute-type from-type to-type t)))
            (($ tuple-type t) (make-tuple-type (map (lambda (x) (substitute-type from-type to-type x)) t)))
            (t t))))
    (map (match-lambda (($ constraint left-type right-type) (make-constraint (substitute-type from-type to-type left-type)
                                                                             (substitute-type from-type to-type right-type)))) constraints))
  
  ; TODO:
  ; - what to do with uneven applications?
  ; - need to still perform type substs on original types
  
  (define (unify-constraints constraints)
    (match constraints
      ((($ constraint left-type right-type) . rest)
       (cond ((equal? left-type right-type)
              (unify-constraints rest))
             ((and (type-variable? left-type)
                   (not (contains-type? right-type left-type)))
              (cons (list left-type right-type)
                    (unify-constraints (substitute-constraints-type rest left-type right-type))))
             ((and (type-variable? right-type)
                   (not (contains-type? left-type right-type)))
              (cons (list right-type left-type)
                    (unify-constraints (substitute-constraints-type rest right-type left-type))))
             ((and (function-type? left-type)
                   (function-type? right-type)
                   #;(equal? (length (function-type-types left-type)) (length (function-type-types right-type))))
              (unify-constraints (append (zip-with make-constraint (function-type-types left-type) (function-type-types right-type)) rest)))
             (else (error 'unify-constraints "cannot unify constraint: ~a = ~a" left-type right-type))))
      (() null)))
  
  (define uc unify-constraints)
  
  (define tests
    (list))
  
  (define (run-all-tests)
    (run-tests (lambda (x) (set! type-variable-count 0) (unify-constraints (reconstruct-types null x))) tests)))