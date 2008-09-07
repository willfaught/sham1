(module Compiler mzscheme
  (require (only (lib "1.ss" "srfi") list-tabulate partition unzip2 zip)
           (only (lib "71.ss" "srfi") values->list)
           (lib "Boundaries.ss" "sham")
           (lib "contract.ss")
           (only (lib "list.ss") foldl foldr)
           (lib "list.ss" "sham" "haskell")
           (lib "match.ss")
           (prefix /c (lib "CoreSyntax.ss" "sham" "haskell"))
           (lib "type-checker.ss" "sham" "haskell")
           (prefix t/ (lib "Types.ss" "sham"))
           (lib "parsers.ss" "sham" "haskell"))
  
  (provide compile)
  
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
  
  ; compile :: CoreSyntax -> datum
  (define (compile syntax)
    (match syntax
      (($ c/Application r d) `(,(compile r) (delay ,(compile d))))
      (($ c/Character v) (string-ref c 0))
      ((? c/Data? x) (compile-data-term x))
      (($ c/Float v) (string->number v))
      (($ c/Function p b) `(lambda (,(string->symbol (string-append "haskell:" p))) ,(compile b)))
      (($ c/Haskell t n) `(contract ,(contractH t) ,(convertHS t (compile term) 1) 'haskell 'scheme))
      (($ c/If g t e) `(if (equal? ,(compile g) (force haskell:True)) ,(compile t) ,(compile e)))
      (($ c/Integer v) (string->number v))
      ((? c/Let? x) (compileLet x))
      (($ c/ListConstructor) 'null)
      ((? c/ML? x) (compileML x))
      ((? c/Module? x) (compile-module-term x))
      (($ c/Scheme t n) `(contract ,(contractS t) ,(convertSH t (string->symbol n) 1) 'scheme 'haskell))
      (($ c/TupleConstructor a) (compileTupleConstructor a))
      (($ c/UnitConstructor) (vector-immutable))
      (($ c/Variable n) `(force ,(string->symbol (string-append "haskell:" n))))))
  
  ; compileLet :: c/Let -> datum
  (define (compileLet syntax)
    (define (compileDeclaration syntax)
      (match syntax
        (($ c/Declaration n b) `(,(string->symbol (string->append "haskell:" n)) (delay ,(compile b))))))
    (match syntax
      (($ c/Let d b) `(letrec ,(map compileDeclaration d) ,(compile b)))))
  
  ; compileML :: c/ML -> datum
  (define (compileML term)
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
        (($ type-variable i) `(make-type-variable ,i))))
    (match-let ((($ ml-term t i) term))
      `(let ((x (assoc ,i ml:types)))
         (if (and (not (equal? x #f))
                  (primitive:type-less-general? ,(type->datum (translate-type-constructor t)) (list-ref x 1)))
             ,(ml->haskell t (string->symbol i) 1)
             (error "Haskell and ML type mismatch")))))
  
  ; compileTupleConstructor :: integer -> datum
  (define (compileTupleConstructor a)
    (define (enumerate n)
      (if (= n 0) null (cons n (enumerate (- n 1)))))
    (define (nest n)
      (if (= n (+ a 1))
          `(vector-immutable ,@(map (lambda (x) (strings->symbol "x" (number->string x))) (reverse (enumerate a))))
          `(lambda (,(strings->symbol "x" (number->string n))) ,(nest (+ n 1)))))
    (nest 1))
  
  ; contractH :: Type -> contract
  (define (contractH type)
    (match type
      (($ t/Application r d) )
      (($ t/Constructor n) 'any/c)
      (($ t/Variable n) 'any/c)
      (($ t/Function) )
      (($ t/List) )
      (($ t/Tuple) )
      (($ t/Unit) )
      TODO
      (($ character-type) `any/c)
      (($ float-type) `any/c)
      (($ function-type p r) `(-> ,(contractS p) ,(contractH r)))
      (($ integer-type) `any/c)
      (($ list-type _) `any/c)
      (($ tuple-type _) `any/c)
      (($ type-constructor _) `any/c)
      (($ type-variable _) `any/c)))
  
  ; contractS :: Type -> contract
  (define (contractS type)
    (match type
      (($ character-type) `(flat-contract char?))
      (($ float-type) `(flat-contract number?))
      (($ function-type p r) `(-> ,(contractH p) ,(contractS r)))
      (($ integer-type) `(flat-contract integer?))
      (($ list-type type) `(and/c (listof ,(contractS type))
                                  (flat-contract proper-list?)
                                  (flat-contract (lambda (x) (not (circular-list? x))))))
      (($ tuple-type types) `(vector/c ,@(map contractS types)))
      (($ type-constructor i) (match i
                                ("Char" (contractS (make-character-type)))
                                ("Float" (contractS (make-float-type)))
                                ("Int" (contractS (make-integer-type)))
                                (_ `(flat-contract ,(strings->symbol "haskell-type:" i "?")))))
      (($ type-variable _) `any/c)))
  
  ; strings->symbol :: string... -> symbol
  ;(define strings->symbol (lambda x (string->symbol (foldl (lambda (x y) (string-append y x)) "" x))))
  )