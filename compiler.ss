; known issues:
; - declarations shadowing prelude declarations don't work
; - cannot enforce arguments corresponding with the same type variable to have the same type using any/c

(module compiler mzscheme
  (require (only (lib "1.ss" "srfi") list-tabulate partition unzip2 zip)
           (only (lib "71.ss" "srfi") values->list)
           (lib "contract.ss")
           (only (lib "list.ss") foldl foldr)
           (lib "list.ss" "haskell")
           (lib "match.ss")
           (lib "terms.ss" "haskell")
           (lib "type-checker.ss" "haskell")
           (lib "types.ss" "haskell"))
  
  (provide compile-term compile-module)
  
  ; characters :: immutable-hash-table
  (define characters
    (make-immutable-hash-table `() 'equal))
  
  ; compile-application-term :: term [term] -> datum
  (define (compile-application-term f a)
    (if (null? a) (compile-term f) `(,(compile-application-term f (cdr a)) (delay ,(compile-term (car a))))))
  
  ; compile-data-term : data-term -> (datum)
  (define (compile-data-term d)
    ; enumerate-identifiers :: integer -> datum
    (define (enumerate-identifiers n)
      (if (equal? n 0) null (cons (strings->symbol "x" n) (enumerate-identifiers (- n 1)))))
    ; nest-functions :: datum integer -> datum
    (define (nest-functions x n)
      (if (equal? n 0) x `(lambda (,(strings->symbol "x" n)) ,(nest-functions x (- n 1)))))
    ; compile-constructor :: string integer -> datum
    (define (compile-constructor i n)
      (let ((m (strings->symbol "make-haskell:" i)))
        `(define ,(strings->symbol "haskell:" i) (if (equal? n 0) m (nest-functions `(,m ,(enumerate-identifiers n)) n)))))
    ; compile-constructor-predicate :: string -> datum
    (define (compile-constructor-predicate i)
      `(define ,(strings->symbol "haskell:is" i) (delay ,(strings->symbol "haskell:" i "?"))))
    ; compile-field :: data-field-term -> datum
    (define (compile-field ci f)
      (match f (($ data-field-term fi _) `(define ,(strings->symbol "haskell:" fi) (lambda (x) (force (,(strings->symbol "haskell:" ci "-" fi) (force x))))))))
    ; compile-data-constructor-term :: data-constructor-term -> (datum)
    (define (compile-data-constructor-term c)
      (match-let ((($ data-constructor-term i f) c))
        (append (list (compile-constructor i (length f)) (compile-constructor-predicate i)) (map (lambda (x) (compile-field i x)) f))))
    (foldl append null (map compile-data-constructor-term (data-term-constructors d))))
  
  ; compile-let-term :: [declaration-term] term -> datum
  (define (compile-let-term d e)
    (define compile-declaration-term
      (match-lambda (($ declaration-term p e)
                     `(,(strings->symbol "haskell:" (car p)) (delay ,(if (null? (cdr p)) (compile-term e) (compile-term (make-function-term (cdr p) e))))))))
    `(letrec ,(map compile-declaration-term d) ,(compile-term e)))
  
  ; compile-module :: module-term -> datum
  (define (compile-module m)
    ; compile-declaration-term :: declaration-term -> datum
    (define compile-declaration-term
      (match-lambda
        (($ declaration-term p e)
         `(define ,(string->symbol (car p)) (delay ,(if (null? (cdr p)) (compile-term e) (compile-term (make-function-term (cdr p) e))))))))
    ; scheme-declaration :: (string type) -> declaration-term
    (define scheme-declaration
      (match-lambda ((i t) (make-declaration-term (list (string-append "scheme:" i)) (make-haskell-term t (make-identifier-term i))))))
    (match-let* ((($ module-term mi md) m)
                 ((da de) (values->list (partition data-term? md)))
                 (dc (foldl append null (map data-context da)))
                 (mc (module-context dc (make-module-term mi de)))
                 (c (append dc mc))
                 ((di _) (values->list (unzip2 c)))
                 (sd (map scheme-declaration c))
                 (hd (map (match-lambda (($ declaration-term (i . r) e) (make-declaration-term (cons (string-append "haskell:" i) r) e))) de)))
      `(module ,(string->symbol mi) mzscheme
         (require (only (lib "1.ss" "srfi") circular-list? proper-list?)
                  (lib "contract.ss")
                  (only (lib "list.ss") foldr)
                  (lib "primitives.ss" "haskell"))
         (provide ,@(map (lambda (x) `(rename ,(string->symbol (string-append "scheme:" x)) ,(string->symbol x))) di))
         ,@(foldl append null (map compile-data-term da))
         ,@(map compile-declaration-term (append sd hd)))))
  
  ; compile-term :: term -> datum
  (define (compile-term term)
    (match term
      (($ application-term f a) (compile-application-term f (reverse a)))
      (($ character-term c) (car (hash-table-get characters c (lambda () (list (string-ref c 0))))))
      (($ float-term f) (string->number f))
      (($ function-term p b) (if (null? p) (compile-term b) `(lambda (,(strings->symbol "haskell:" (car p))) ,(compile-term (make-function-term (cdr p) b)))))
      (($ haskell-term type term) `(contract ,(haskell-contract type) ,(haskell->scheme type (compile-term term) 1) 'haskell 'scheme))
      (($ identifier-term i) (let ((x (assoc i prelude))) (if x (list-ref x 1) `(force ,(strings->symbol "haskell:" i)))))
      (($ if-term g t e) `(if ,(compile-term g) ,(compile-term t) ,(compile-term e)))
      (($ integer-term i) (string->number i))
      (($ let-term d e) (compile-let-term d e))
      (($ list-term e) (if (null? e) null `(cons-immutable (delay ,(compile-term (car e))) (delay ,(compile-term (make-list-term (cdr e)))))))
      (($ scheme-term type identifier) (scheme->haskell type `(contract ,(scheme-contract type) ,(string->symbol identifier) 'scheme 'haskell) 1))
      (($ tuple-term e) (compile-term (make-application-term (make-tuplecon-term (length e)) e)))
      (($ tuplecon-term a) (compile-tuplecon-term a))))
  
  ; compile-tuplecon-term :: integer -> datum
  (define (compile-tuplecon-term a)
    (define (enumerate n)
      (if (= n 0) null (cons n (enumerate (- n 1)))))
    (define (nest n)
      (if (= n (+ a 1))
          `(vector-immutable ,@(map (lambda (x) (strings->symbol "x" (number->string x))) (reverse (enumerate a))))
          `(lambda (,(strings->symbol "x" (number->string n))) ,(nest (+ n 1)))))
    (nest 1))
  
  ; data-context :: data-term -> ((string type))
  (define (data-context d)
    ; field-context :: data-field-term type-constructor -> ((string type))
    (define (field-context f dt)
      (match-let ((($ data-field-term i ft) f))
        (list i (make-function-type dt ft))))
    ; constructor-context :: data-constructor-term type-constructor -> ((string type))
    (define (constructor-context c t)
      (match-let ((($ data-constructor-term i f) c))
        (append (list (list i (foldr make-function-type t (map (match-lambda (($ data-field-term _ t) t)) f)))
                      (list (if (equal? (string-ref i 0) #\:)
                                (string-append i "?")
                                (string-append "is" i))
                            (make-function-type t (make-type-constructor "Bool"))))
                (map (lambda (x) (field-context x t)) f))))
    (match-let ((($ data-term t c) d))
      (foldl append null (map (lambda (x) (constructor-context x t)) c))))
  
  ; haskell-contract :: type -> contract
  (define (haskell-contract type)
    (match type
      (($ boolean-type) `any/c)
      (($ character-type) `any/c)
      (($ float-type) `any/c)
      (($ function-type p r) `(-> ,(scheme-contract p) ,(haskell-contract r)))
      (($ integer-type) `any/c)
      (($ list-type _) `any/c)
      (($ tuple-type _) `any/c)
      (($ type-constructor _) `any/c)
      (($ type-variable _) `any/c)))
  
  ; haskell->scheme :: type term integer -> datum
  (define (haskell->scheme type term depth)
    (let ((id `(lambda (x) x)))
      (match type
        (($ boolean-type) term)
        (($ character-type) term)
        (($ float-type) term)
        (($ function-type p r) (let ((i (identifier depth)))
                                 `(lambda (,i) ,(haskell->scheme r `(,term (delay ,(scheme->haskell p i (+ depth 1)))) (+ depth 1)))))
        (($ integer-type) term)
        (($ list-type _) term)
        (($ tuple-type _) term)
        (($ type-variable _) term))))
  
  ; identifier :: integer -> symbol
  (define (identifier n)
    (string->symbol (string-append "x" (number->string n))))
  
  ; prelude :: immutable-hash-table
  (define prelude `((":" primitive:list-cons)
                    ("head" primitive:list-head)
                    ("tail" primitive:list-tail)
                    #;("null" primitive:list-null)
                    ("fst" primitive:tuple-first)
                    ("snd" primitive:tuple-second)))
  
  ; scheme-contract :: type -> contract
  (define (scheme-contract type)
    (match type
      (($ boolean-type) `(flat-contract boolean?))
      (($ character-type) `(flat-contract char?))
      (($ float-type) `(flat-contract number?))
      (($ function-type p r) `(-> ,(haskell-contract p) ,(scheme-contract r)))
      (($ integer-type) `(flat-contract integer?))
      (($ list-type type) `(and/c (listof ,(scheme-contract type))
                                  (flat-contract proper-list?)
                                  (flat-contract (lambda (x) (not (circular-list? x))))))
      (($ tuple-type types) `(vector/c ,@(map scheme-contract types)))
      (($ type-constructor i) (scheme-contract (translate-type-constructor (make-type-constructor i))))
      (($ type-variable _) `any/c)))
  
  ; scheme->haskell :: type term integer -> datum
  (define (scheme->haskell type term depth)
    (let ((id `(lambda (x) x)))
      (match type
        (($ boolean-type) term)
        (($ character-type) term)
        (($ float-type) term)
        (($ function-type p r) (let ((i (identifier depth)))
                                 `(lambda (,i) ,(scheme->haskell r `(,term ,(haskell->scheme p i (+ depth 1))) (+ depth 1)))))
        (($ integer-type) term)
        (($ list-type type) `(foldr (lambda (x y) (cons-immutable (delay ,(scheme->haskell type `x depth)) (delay y))) null ,term))
        (($ tuple-type types) (let* ((pairs (zip types (list-tabulate (length types) (lambda (x) x))))
                                     (elements (map (match-lambda ((type index) `(delay ,(scheme->haskell type `(vector-ref x ,index) depth)))) pairs)))
                                `(let ((x ,term)) (vector-immutable ,@elements))))
        (($ type-variable _) term))))
  
  ; strings->symbol :: string... -> symbol
  (define strings->symbol (lambda x (string->symbol (foldl (lambda (x y) (string-append y x)) "" x)))))