(module typechecker mzscheme
  (require (only (lib "1.ss" "srfi") alist-cons delete-duplicates filter make-list unzip2 zip)
           (lib "compiler.ss" "haskell")
           (lib "prelude.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "types.ss" "haskell")
           (only (lib "list.ss") foldl foldr)
           (lib "match.ss")
           ;(lib "test.ss" "haskell")
           (lib "reader.ss" "haskell")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide module-declaration-types)
  
  (define-struct constraint (left-type right-type) #f)
  
  ; module-declaration-types :: module-term -> [type]
  (define (module-declaration-types module)
    (match (reconstruct-types null module)
      ((types constraints) (let ((substitution (unify-constraints constraints)))
                             (map (lambda (x) (substitute-types substitution x)) types)))))
  
  ; lunzip2 :: [(a, b)] -> ([a], [b])
  (define (lunzip2 x)
    (let-values (((x y) (unzip2 x))) (list x y)))
  
  ; zip-with :: (a -> b -> c) -> [a] -> [b] -> [c]
  (define (zip-with f x y)
    (if (equal? (length x) (length y))
        (if (null? x) null (cons (f (car x) (car y)) (zip-with f (cdr x) (cdr y))))
        (error 'zip-with "lists have different lengths")))
  
  ; reconstruct-types :: [(string, type)] -> term -> (type, [constraint])
  (define (reconstruct-types context term)
    (match term
      (($ application-term f a) (match-let* (((f-type f-constraints) (reconstruct-types context f))
                                             ((a-types a-constraints) (lunzip2 (map (lambda (x) (reconstruct-types context x)) a)))
                                             (type (fresh-type-variable))
                                             (constraints (cons (make-constraint f-type (make-function-type (append a-types (list type))))
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
                               (list (make-function-type (append p-types (list type))) constraints)))
      (($ identifier-term i) (if (member i prelude-declarations)
                                 (let ((type (hash-table-get prelude-types i)))
                                   (if (universal-type? type)
                                       (list (instantiate type) null)
                                       (list type null)))
                                 (match (assoc i context)
                                   ((_ type) (if (universal-type? type)
                                                 (list (instantiate type) null)
                                                 (list type null)))
                                   (_ (error 'reconstruct-types "Not in scope: '~a'" i)))))
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
                             (list (make-function-type (append types (list (make-tuple-type types)))) null)))))
  
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
        (($ function-type t) (foldr append null (map type-variables t)))
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
          (($ function-type t) (foldl (lambda (x y) (or x y)) #f (map (lambda (x) (contains-type? x containee-type)) t)))
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
    ; zip-function-types :: ([type], [type]) -> [constraint]
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
             ((and (list-type? left-type)
                   (list-type? right-type))
              (unify-constraints (cons (make-constraint (list-type-type left-type) (list-type-type right-type)) rest)))
             ((and (tuple-type? left-type)
                   (tuple-type? right-type)
                   (equal? (length (tuple-type-types left-type)) (length (tuple-type-types right-type))))
              (unify-constraints (append (zip-with make-constraint (tuple-type-types left-type) (tuple-type-types right-type)) rest)))
             (else (error 'unify-constraints "cannot unify the constraint: ~a = ~a" left-type right-type))))
      (() null)))
  
  (define test-expression-parser (expression-parser "test"))
  
  (define test-type-parser (type-parser "test"))
  
  (define (parse-expression expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (test-expression-parser (lambda () (language-lexer port)))))
  
  (define (parse-type type)
    (let ((port (open-input-string type)))
      (port-count-lines! port)
      (test-type-parser (lambda () (language-lexer port)))))
  #;(
  (define (run-test-suites)
    (let ((parser (expression-parser "test")))))
      
  
  (define (character-test-suite type
    (test-suite "character"
                (test-case "1"
                           (check-equal? )))))
  )
  #;(define tests
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
                     (make-list-term (list (make-float-term "1.2")
                                           (make-float-term "3.4")))
                     (make-list-type (make-float-type)))
          (make-test "tuple-term 1"
                     (make-tuple-term (list (make-character-term "a")
                                            (make-float-term "1.2")))
                     (make-tuple-type (list (make-character-type)
                                            (make-float-type))))
          (make-test "tuple-term 2"
                     (make-tuple-term (list (make-character-term "a")
                                            (make-float-term "1.2")
                                            (make-integer-term "3")))
                     (make-tuple-type (list (make-character-type)
                                            (make-float-type)
                                            (make-integer-type))))
          (make-test "tuplecon-term 1"
                     (make-tuplecon-term 2)
                     (make-function-type (list (make-type-variable "t1")
                                               (make-type-variable "t2")
                                               (make-tuple-type (list (make-type-variable "t1")
                                                                      (make-type-variable "t2"))))))
          (make-test "tuplecon-term 2"
                     (make-tuplecon-term 3)
                     (make-function-type (list (make-type-variable "t1")
                                               (make-type-variable "t2")
                                               (make-type-variable "t3")
                                               (make-tuple-type (list (make-type-variable "t1")
                                                                      (make-type-variable "t2")
                                                                      (make-type-variable "t3"))))))
          (make-test "let-term 1"
                     (make-let-term (list (make-declaration-term (list "a")
                                                                 (make-character-term "a")))
                                    (make-float-term "1.2"))
                     (make-float-type))
          (make-test "let-term 2"
                     (make-let-term (list (make-declaration-term (list "a")
                                                                 (make-character-term "a")))
                                    (make-identifier-term "a"))
                     (make-character-type))
          (make-test "let-term 3"
                     (make-let-term (list (make-declaration-term (list "a")
                                                                 (make-identifier-term "a")))
                                    (make-identifier-term "a"))
                     (make-type-variable "t2"))
          (make-test "let-term 4"
                     (make-let-term (list (make-declaration-term (list "a")
                                                                 (make-character-term "a"))
                                          (make-declaration-term (list "b")
                                                                 (make-identifier-term "a")))
                                    (make-identifier-term "b"))
                     (make-character-type))
          (make-test "let-term 5"
                     (make-let-term (list (make-declaration-term (list "a")
                                                                 (make-identifier-term "b"))
                                          (make-declaration-term (list "b")
                                                                 (make-character-term "a")))
                                    (make-identifier-term "a"))
                     (make-character-type))
          (make-test "let-term 6"
                     (make-let-term (list (make-declaration-term (list "a" "x")
                                                                 (make-character-term "a")))
                                    (make-identifier-term "a"))
                     (make-function-type (list (make-type-variable "t3")
                                               (make-character-type))))
          (make-test "let-term 7"
                     (make-let-term (list (make-declaration-term (list "a" "x")
                                                                 (make-identifier-term "x")))
                                    (make-identifier-term "a"))
                     (make-function-type (list (make-type-variable "t3")
                                               (make-type-variable "t3"))))
          (make-test "let-term 8"
                     (make-let-term (list (make-declaration-term (list "a" "x" "y")
                                                                 (make-identifier-term "y")))
                                    (make-identifier-term "a"))
                     (make-function-type (list (make-type-variable "t4")
                                               (make-type-variable "t5")
                                               (make-type-variable "t5"))))
          (make-test "let-term 9"
                     (make-let-term (list (make-declaration-term (list "a" "x")
                                                                 (make-identifier-term "x"))
                                          (make-declaration-term (list "b")
                                                                 (make-application-term (make-identifier-term "a")
                                                                                        (list (make-character-term "a"))))
                                          (make-declaration-term (list "c")
                                                                 (make-application-term (make-identifier-term "a")
                                                                                        (list (make-character-term "b")))))
                                    (make-identifier-term "a"))
                     (make-function-type (list (make-character-type)
                                               (make-character-type))))
          (make-test "let-term 10"
                     (make-let-term (list (make-declaration-term (list "a" "x")
                                                                 (make-integer-term "1")))
                                    (make-application-term (make-identifier-term "a")
                                                           (list (make-character-term "a"))))
                     (make-integer-type))
          (make-test "let-term 11"
                     (make-let-term (list (make-declaration-term (list "a" "x")
                                                                 (make-identifier-term "x")))
                                    (make-application-term (make-identifier-term "a")
                                                           (list (make-character-term "a"))))
                     (make-character-type))
          (make-test "let-term 12"
                     (make-let-term (list (make-declaration-term (list "a" "x")
                                                                 (make-identifier-term "x")))
                                    (make-let-term (list (make-declaration-term (list "b") (make-application-term (make-identifier-term "a")
                                                                                                                  (list (make-character-term "b"))))
                                                         (make-declaration-term (list "c") (make-application-term (make-identifier-term "a")
                                                                                                                  (list (make-float-term "1.2")))))
                                                   (make-identifier-term "a")))
                     (make-function-type (list (make-type-variable "t9")
                                               (make-type-variable "t9"))))
          (make-test "let-term 13"
                     (make-let-term (list (make-declaration-term (list "a" "x")
                                                                 (make-identifier-term "x")))
                                    (make-let-term (list (make-declaration-term (list "b") (make-application-term (make-identifier-term "a")
                                                                                                                  (list (make-character-term "b"))))
                                                         (make-declaration-term (list "c") (make-application-term (make-identifier-term "a")
                                                                                                                  (list (make-float-term "1.2")))))
                                                   (make-identifier-term "b")))
                     (make-character-type))
          (make-test "function-term 1"
                     (make-function-term (list "x")
                                         (make-character-term "a"))
                     (make-function-type (list (make-type-variable "t1")
                                               (make-character-type))))
          (make-test "function-term 2"
                     (make-function-term (list "x")
                                         (make-identifier-term "x"))
                     (make-function-type (list (make-type-variable "t1")
                                               (make-type-variable "t1"))))
          (make-test "function-term 3"
                     (make-function-term (list "x" "y")
                                         (make-identifier-term "y"))
                     (make-function-type (list (make-type-variable "t1")
                                               (make-type-variable "t2")
                                               (make-type-variable "t2"))))
          (make-test "function-term 4"
                     (make-function-term (list "x" "y")
                                         (make-tuple-term (list (make-identifier-term "x")
                                                                (make-identifier-term "y"))))
                     (make-function-type (list (make-type-variable "t1")
                                               (make-type-variable "t2")
                                               (make-tuple-type (list (make-type-variable "t1") (make-type-variable "t2"))))))
          (make-test "application-term 1"
                     (make-application-term (make-function-term (list "x")
                                                                (make-character-term "a"))
                                            (list (make-float-term "1.2")))
                     (make-character-type))
          (make-test "application-term 2"
                     (make-application-term (make-function-term (list "x")
                                                                (make-identifier-term "x"))
                                            (list (make-character-term "a")))
                     (make-character-type))
          (make-test "application-term 3"
                     (make-application-term (make-function-term (list "x" "y")
                                                                (make-identifier-term "y"))
                                            (list (make-character-term "a")
                                                  (make-float-term "1.2")))
                     (make-float-type))
          (make-test "application-term 4"
                     (make-application-term (make-function-term (list "x" "y")
                                                                (make-tuple-term (list (make-identifier-term "x")
                                                                                       (make-identifier-term "y"))))
                                            (list (make-character-term "b")
                                                  (make-float-term "1.2")))
                     (make-tuple-type (list (make-character-type)
                                            (make-float-type))))
          ))
  
  #;(define (run-all-tests)
    (run-tests (lambda (x)
                 (match-let* (((type constraints) (reconstruct-types null x)))
                   (substitute-types (unify-constraints constraints) type)))
               tests)))