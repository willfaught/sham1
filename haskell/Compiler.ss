(module Compiler mzscheme
  (require (only (lib "1.ss" "srfi") list-tabulate partition unzip2 zip)
           (only (lib "71.ss" "srfi") values->list)
           (lib "boundaries.ss" "sham")
           (lib "contract.ss")
           (only (lib "list.ss") foldl foldr)
           (lib "list.ss" "sham" "haskell")
           (lib "match.ss")
           (lib "CoreSyntax.ss" "sham" "haskell")
           (lib "type-checker.ss" "sham" "haskell")
           (lib "Types.ss" "sham")
           (lib "parsers.ss" "sham" "haskell"))
  
  (provide compile-data-term compile-term)
  
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
  
  ; compile-import-term :: import-term -> datum
  (define (compile-import-term i)
    (let* ((p (import-term-path i)))
      (if (not (file-exists? p))
          (error 'compile-import-term "error: ~a is not a file" p)
          (let ((f `(file ,p))
                (a (import-term-alias i)))
            (if (not (equal? a #f)) `(prefix ,(string->symbol a) ,f) f)))))
  
  ; compile-let-term :: (declaration-term) term -> datum
  (define (compile-let-term d e)
    (define compile-declaration-term
      (match-lambda (($ declaration-term p e)
                     `(,(strings->symbol "haskell:" (car p)) (delay ,(if (null? (cdr p)) (compile-term e) (compile-term (make-function-term (cdr p) e))))))))
    `(letrec ,(map compile-declaration-term d) ,(compile-term e)))
  
  ; compile-ml-term :: ml-term -> datum
  (define (compile-ml-term term)
    (match-let ((($ ml-term t i) term))
      `(let ((x (assoc ,i ml:types)))
         (if (and (not (equal? x #f))
                  (primitive:type-less-general? ,(type->datum (translate-type-constructor t)) (list-ref x 1)))
             ,(ml->haskell t (string->symbol i) 1)
             (error "Haskell and ML type mismatch")))))
  
  ; compile-module-term :: module-term -> datum
  (define (compile-module-term m)
    ; compile-declaration-term :: declaration-term -> datum
    (define compile-declaration-term
      (match-lambda
        (($ declaration-term p e)
         `(define ,(string->symbol (car p)) (delay ,(if (null? (cdr p)) (compile-term e) (compile-term (make-function-term (cdr p) e))))))))
    ; scheme-declaration :: (string type) -> declaration-term
    (define scheme-declaration
      (match-lambda ((i t) (make-declaration-term (list (string-append "scheme:" i)) (make-haskell-term t (make-identifier-term i))))))
    (match-let* ((($ module-term mi mim md) m)
                 ((da de) (values->list (partition data-term? md)))
                 (mc (module-context null m))
                 ((di _) (values->list (unzip2 mc)))
                 #;(sd (map scheme-declaration mc))
                 (hd (map (match-lambda (($ declaration-term (i . r) e) (make-declaration-term (cons (string-append "haskell:" i) r) e))) de)))
      `(module ,(string->symbol mi) mzscheme
         (require (only (lib "1.ss" "srfi") circular-list? proper-list?)
                  (lib "contract.ss")
                  (only (lib "list.ss") foldl foldr)
                  (lib "primitives.ss" "sham" "haskell")
                  (lib "types.ss" "sham" "haskell")
                  ,@(map compile-import-term mim))
         (provide ,@(map (lambda (x) `(rename ,(string->symbol (string-append "haskell:" x)) ,(string->symbol x))) di))
         (define-struct lump (contents))
         ,@(foldl append null (map compile-data-term da))
         ,@(map compile-declaration-term hd))))
  
  ; compile-term :: term -> datum
  (define (compile-term term)
    (match term
      (($ application-term f a) (compile-application-term f (reverse a)))
      (($ character-term c) (car (hash-table-get characters c (lambda () (list (string-ref c 0))))))
      (($ float-term f) (string->number f))
      (($ function-term p b) (if (null? p) (compile-term b) `(lambda (,(strings->symbol "haskell:" (car p))) ,(compile-term (make-function-term (cdr p) b)))))
      (($ haskell-term type term) `(contract ,(haskell-contract type) ,(haskell->scheme type (compile-term term) 1) 'haskell 'scheme))
      (($ identifier-term i) `(force ,(strings->symbol "haskell:" i)))
      (($ if-term g t e) `(if (equal? ,(compile-term g) (force haskell:True)) ,(compile-term t) ,(compile-term e)))
      (($ integer-term i) (string->number i))
      (($ let-term d e) (compile-let-term d e))
      (($ list-term e) (if (null? e) null `(cons (delay ,(compile-term (car e))) (delay ,(compile-term (make-list-term (cdr e)))))))
      ((? ml-term? x) (compile-ml-term x))
      ((? module-term? m) (compile-module-term m))
      (($ scheme-term type identifier) `(contract ,(scheme-contract type) ,(scheme->haskell type (string->symbol identifier) 1) 'scheme 'haskell))
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
      (($ character-type) `any/c)
      (($ float-type) `any/c)
      (($ function-type p r) `(-> ,(scheme-contract p) ,(haskell-contract r)))
      (($ integer-type) `any/c)
      (($ list-type _) `any/c)
      (($ tuple-type _) `any/c)
      (($ type-constructor _) `any/c)
      (($ type-variable _) `any/c)))
  
  ; scheme-contract :: type -> contract
  (define (scheme-contract type)
    (match type
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
  
  ; strings->symbol :: string... -> symbol
  (define strings->symbol (lambda x (string->symbol (foldl (lambda (x y) (string-append y x)) "" x))))
  
  ; type->datum :: type -> datum
  (define (type->datum type)
    (match type
      (($ character-type) `(make-character-type))
      (($ float-type) `(make-float-type))
      (($ function-type p r) `(make-function-type ,(type->datum p) ,(type->datum r)))
      (($ integer-type) `(make-integer-type))
      (($ list-type t) `(make-list-type ,(type->datum t)))
      (($ tuple-type t) `(make-tuple-type (list ,@(map type->datum t))))
      (($ type-constructor i) `(make-type-constructor ,i))
      (($ type-variable i) `(make-type-variable ,i)))))