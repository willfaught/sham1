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
           (lib "types.ss" "haskell")
           (lib "parsers.ss" "haskell"))
  
  (provide compile-data-term compile-term compile-module)
  
  ; characters :: immutable-hash-table
  (define characters
    (make-immutable-hash-table `() 'equal))
  
  ; compile-application-term :: term (term) -> datum
  (define (compile-application-term f a)
    (if (null? a) (compile-term f) `(,(compile-application-term f (cdr a)) (delay ,(compile-term (car a))))))
  
  ; compile-constructor-term :: string constructor-term -> (datum)
  (define (compile-constructor-term ct c)
    ; constructor :: string integer -> datum
    (define (constructor i n)
      ; enumerate-identifiers :: integer -> (symbol)
      (define (enumerate-identifiers n)
        (if (equal? n 0) null (cons (strings->symbol "x" (number->string n)) (enumerate-identifiers (- n 1)))))
      ; nest-functions :: datum integer -> datum
      (define (nest-functions x n)
        (if (equal? n 0) x `(lambda (,(strings->symbol "x" (number->string n))) ,(nest-functions x (- n 1)))))
      (let* ((di (strings->symbol "haskell:" i))
             (m (strings->symbol "make-haskell-constructor:" i))
             (db `(delay ,(if (equal? n 0) `(,m) (nest-functions `(,m ,@(enumerate-identifiers n)) n)))))
        `(define ,di ,db)))
    ; constructor-type :: string (string) -> datum
    (define (constructor-type ci fi)
      (let ((di (strings->symbol "haskell-constructor:" ci))
            (dt (strings->symbol "haskell-type:" ct))
            (df (map (lambda (x) (string->symbol x)) fi)))
        `(define-struct (,di ,dt) ,df #f)))
    ; predicate :: string -> datum
    (define (predicate i)
      (let ((di (strings->symbol "haskell:is" i))
            (db `(delay (lambda (x) (if (,(strings->symbol "haskell-constructor:" i "?") (force x))
                                        (force haskell:True)
                                        (force haskell:False))))))
        `(define ,di ,db)))
    (match-let* ((($ constructor-term ci cf) c)
                 (fi (map (match-lambda (($ field-term i _) i)) cf)))
      (append (list (constructor-type ci fi)
                    (constructor ci (length cf))
                    (predicate ci))
              (map (lambda (x) (compile-field-term ci x)) cf))))
  
  ; compile-data-term : data-term -> (datum)
  (define (compile-data-term d)
    ; data-type :: string -> datum
    (define (data-type i)
      (let ((ti (strings->symbol "haskell-type:" i)))
        `(define-struct ,ti () #f)))
    (match-let ((($ data-term di dc) d))
      (append (list (data-type di))
              (foldl append null (map (lambda (x) (compile-constructor-term di x)) dc)))))
  
  ; compile-field-term :: string field-term -> datum
  (define (compile-field-term ci f)
    (match-let* ((($ field-term fi _) f)
                 (di (strings->symbol "haskell:" fi))
                 (db `(delay (lambda (x) (force (,(strings->symbol "haskell-constructor:" ci "-" fi) (force x)))))))
      `(define ,di ,db)))
  
  ; compile-let-term :: (declaration-term) term -> datum
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
                 (mc (module-context null m))
                 ((di _) (values->list (unzip2 mc)))
                 (sd (map scheme-declaration mc))
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
      (($ identifier-term i) (let ((x (assoc i primitives))) (if x (list-ref x 1) `(force ,(strings->symbol "haskell:" i)))))
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
        (($ type-constructor _) term)
        (($ type-variable _) term))))
  
  ; identifier :: integer -> symbol
  (define (identifier n)
    (string->symbol (string-append "x" (number->string n))))
  
  ; primitives :: immutable-hash-table
  (define primitives `((":" primitive:list-cons)
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
      (($ type-constructor i) (match i
                                ("Char" (scheme-contract (make-character-type)))
                                ("Float" (scheme-contract (make-float-type)))
                                ("Int" (scheme-contract (make-integer-type)))
                                (_ `(flat-contract ,(strings->symbol "haskell-type:" i "?")))))
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
        (($ type-constructor _) term)
        (($ type-variable _) term))))
  
  ; strings->symbol :: string... -> symbol
  (define strings->symbol (lambda x (string->symbol (foldl (lambda (x y) (string-append y x)) "" x)))))