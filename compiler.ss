(module compiler mzscheme
  (require #;(only (lib "1.ss" "srfi") )
           (lib "match.ss")
           (lib "prelude.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "test.ss" "haskell"))
  
  #;(provide compile-module)
  
  #;(define (compile-module module declaration-types)
    (define compile-declaration-term
      (match-lambda
        (($ declaration-term p e) `(define ,(string->symbol (car p)) (delay ,(if (null? (cdr p))
                                                                                 (compile-term e)
                                                                                 (compile-term (make-function-term (cdr p) e))))))))
    `(module ,(string->symbol i) mzscheme
       (require (lib "haskell-prelude.ss" "hs")
                (lib "match.ss"))
       (provide (all-defined))
       ,@(map compile-declaration-term d)))
  
  ; compile-term :: term -> [quoted data]
  (define (compile-term term type)
    (match term
      (($ application-term f a) (compile-eapp f (reverse a)))
      #;(($ case-term e as) `(match ,(compile-term e) ,@(map (lambda (a) `(,(string->symbol (car a)) ,(compile-term (cdr a)))) as)))
      (($ character-term c) (car (hash-table-get characters c (lambda () (list (string-ref c 0))))))
      (($ float-term f) (string->number f))
      (($ function-term p b) (if (null? p) (compile-term b) `(lambda (,(string->symbol (car p))) ,(compile-term (make-function-term (cdr p) b)))))
      (($ guard-term type term) `(contract ,(type->contract type) ,term 'haskell 'scheme))
      (($ haskell-term type term) (match type
                                    (($ function-type types) )))
      (($ identifier-term i) (if (member i prelude-declarations) `(force ,(string->symbol (string-append "haskell:" i))) `(force ,(string->symbol i))))
      (($ if-term g t e) `(if ,(compile-term g) ,(compile-term t) ,(compile-term e)))
      (($ integer-term i) (string->number i))
      (($ let-term d e) (compile-let-term d e))
      (($ list-term e) (if (null? e) null `(cons-immutable (delay ,(compile-term (car e))) (delay ,(compile-term (make-list-term (cdr e)))))))
      (($ module-term i d) (compile-module-term i d))
      (($ tuple-term e) (compile-term (make-application-term (make-tuplecon-term (length e)) e)))
      (($ tuplecon-term a) (compile-etupcon a))))
  
  
  ; compile-haskell-term :: type -> term -> [quoted data]
  (define (compile-haskell-term type term)
    (define (identifier n)
      (string->symbol (string-append "x" (number->string n))))
    (define (nest-functions term function-count)
      (if (equal? function-count 0) term `(lambda (,(identifier function-count)) ,(nest-functions term (- function-count 1)))))
    (define (nest-applications term argument-count types)
      (match types
        ((type . rest) (nest-applications `(,term (,(compile-haskell-term type term) (identifier argument-count))) rest (+ argument-count 1)))
        (() term)))
    (let ((id `(lambda (x) x)))
      (match type
        (($ boolean-type) id)
        (($ character-type) id)
        (($ float-type) id)
        (($ function-type types) (nest-functions (nest-applications (compile-term term) 1 types) (length types)))
        (($ integer-type) term)
        (($ list-type _) term))))
  
  ; compile-scheme-term :: type -> term -> [quoted data]
  (define (compile-scheme-term type term)
    (match type
      (($ boolean-type) (compile-term term))
      (($ character-type) (compile-term term))
      (($ float-type) (compile-term term))
      (($ function-type types) (let ((identifiers (list-tabulate (length types) (lambda (x) (string-append "x" (number->string (+ n 1)))))))
                                 (compile-term (make-function-term identifiers (make-application-term term (map make-identifier-term identifiers))))))
      (($ integer-type) (compile-term term))
      (($ list-type type) `(foldr (lambda (x y) (cons (delay ,(compile-term x)) (delay y))) null ,term))
      (($ tuple-type types) (let ((pairs (zip types (list-tabulate (length types) (lambda (x) x)))))
                              `((lambda (x) (vector-immutable ,@(map (match-lambda ((type . index) (compile-scheme-term type `(vector-ref x ,index)))) pairs))) ,term)))))
  
  ; type->contract :: type -> contract
  (define (type->contract type)
    (match type
      (($ boolean-type) `(flat-contract boolean?))
      (($ character-type) `(flat-contract char?))
      (($ float-type) `(flat-contract number?))
      (($ function-type types) `,(foldr1 (lambda (x y) (-> (type->contract x) (type->contract y))) types))
      (($ integer-type) `(flat-contract integer?))
      (($ list-type type) `(list-immutableof ,(type->contract type)))
      (($ tuple-type types) `(vector-immutable/c ,(map type->contract types)))
      (($ type-constructor identifier) (type->contract (translate-type (make-type-constructor identifier))))
      (($ type-variable _) `any/c) ; cannot enforce arguments corresponding with the same type variable to have the same type
      (($ universal-type type) (type->contract type))))
  
  (define (compile-let-term d e)
    (define compile-declaration-term
      (match-lambda (($ declaration-term p e) `(,(string->symbol (car p)) (delay ,(if (null? (cdr p))
                                                                                      (compile-term e)
                                                                                      (compile-term (make-function-term (cdr p) e))))))))
    `(letrec ,(map compile-declaration-term d) ,(compile-term e)))
  
  (define (compile-eapp f a)
    (if (null? a) (compile-term f) `(,(compile-eapp f (cdr a)) (delay ,(compile-term (car a))))))
  
  (define (compile-etupcon a)
    (define (enumerate n)
      (if (= n 0) null (cons n (enumerate (- n 1)))))
    (define (nest n)
      (if (= n (+ a 1))
          `(vector-immutable ,@(map (lambda (x) (string->symbol (string-append "x" (number->string x)))) (reverse (enumerate a))))
          `(lambda (,(string->symbol (string-append "x" (number->string n)))) ,(nest (+ n 1)))))
    (nest 1))
  
  (define characters
    (make-immutable-hash-table `() 'equal))
  
  (define tests
    (list (make-test "eapp 1"
                     (make-application-term (make-identifier-term "x") (list (make-integer-term "4")))
                     '((force x) (delay 4)))
          (make-test "eapp 2"
                     (make-application-term (make-identifier-term "x") (list (make-integer-term "5") (make-integer-term "6")))
                     '(((force x) (delay 5)) (delay 6)))
          (make-test "ecase 1"
                     (make-case-term (make-integer-term "2") (list (cons "x" (make-integer-term "3"))))
                     '(match 2 (x 3)))
          (make-test "echar 1"
                     (make-character-term "c")
                     #\c)
          (make-test "edecl 1"
                     (make-declaration-term (list "x") (make-integer-term "2"))
                     '(define x (delay 2)))
          (make-test "edecl 2"
                     (make-declaration-term (list "x" "y") (make-integer-term "2"))
                     '(define x (delay (match-lambda (y 2)))))
          (make-test "edecl 3"
                     (make-declaration-term (list "x" "_" "y") (make-integer-term "2"))
                     '(define x (delay (match-lambda (_ (match-lambda (y 2)))))))
          (make-test "efun 1"
                     (make-function-term (list "x") (make-identifier-term "x"))
                     '(match-lambda (x (force x))))
          (make-test "efun 2"
                     (make-function-term (list "x" "y") (make-identifier-term "x"))
                     '(match-lambda (x (match-lambda (y (force x))))))
          (make-test "eid 1"
                     (make-identifier-term "x")
                     '(force x))
          (make-test "eid 2"
                     (make-identifier-term "+")
                     '(force haskell:+))
          (make-test "eif 1"
                     (make-if-term (make-identifier-term "True") (make-integer-term "2") (make-integer-term "3"))
                     '(if (force haskell:True) 2 3))
          (make-test "elet 1"
                     (make-let-term (list (make-declaration-term (list "x")
                                                                 (make-integer-term "2")))
                                    (make-integer-term "3"))
                     '(begin (define x (delay 2)) 3))
          (make-test "elet 2"
                     (make-let-term (list (make-declaration-term (list "x")
                                                                 (make-integer-term "2"))
                                          (make-declaration-term (list "y")
                                                                 (make-integer-term "3")))
                                    (make-integer-term "4"))
                     '(begin (define x (delay 2)) (define y (delay 3)) 4))
          (make-test "elist 1"
                     (make-list-term null)
                     '())
          (make-test "elist 2"
                     (make-list-term (list (make-integer-term "2")))
                     '(cons (delay 2) (delay ())))
          (make-test "elist 3"
                     (make-list-term (list (make-integer-term "2") (make-integer-term "3")))
                     '(cons (delay 2) (delay (cons (delay 3) (delay ())))))
          (make-test "enum 1"
                     (make-integer-term "1")
                     1)
          (make-test "etup 1"
                     (make-tuple-term (list (make-integer-term "2") (make-character-term "c")))
                     '(((lambda (x1) (lambda (x2) (vector-immutable x1 x2))) (delay 2)) (delay #\c)))
          (make-test "etupcon 1"
                     (make-tuplecon-term 2)
                     '(lambda (x1) (lambda (x2) (vector-immutable x1 x2))))
          (make-test "etupcon 2"
                     (make-tuplecon-term 3)
                     '(lambda (x1) (lambda (x2) (lambda (x3) (vector-immutable x1 x2 x3)))))))
  
  (define (run-all-tests)
    (run-tests (lambda (x) (compile-term x)) tests)))
