; known issues:
; - declarations shadowing prelude declarations don't work
; - cannot enforce arguments corresponding with the same type variable to have the same type using any/c

(module compiler mzscheme
  (require (only (lib "1.ss" "srfi") list-tabulate zip)
           (lib "contract.ss")
           (only (lib "list.ss") foldr)
           (lib "list.ss" "haskell")
           (lib "match.ss")
           (lib "prelude.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "types.ss" "haskell"))
  
  (provide compile-term compile-module)
  
  ; compile-module :: module-term -> [type] -> datum
  (define (compile-module module declaration-types)
    (define compile-declaration-term
      (match-lambda
        (($ declaration-term p e) `(define ,(string->symbol (car p)) (delay ,(compile-term e))))))
    `(module ,(string->symbol (module-term-identifier module)) mzscheme
       (require (only (lib "1.ss" "srfi") circular-list? proper-list?)
                (lib "contract.ss")
                (lib "Prelude.hs" "haskell"))
       (provide (all-defined))
       (define-struct :lump (value))
       ,@(map compile-declaration-term (module-term-declarations module))))
  
  ; compile-term :: term -> datum
  (define (compile-term term)
    (match term
      (($ application-term f a) (compile-application-term f (reverse a)))
      (($ character-term c) (car (hash-table-get characters c (lambda () (list (string-ref c 0))))))
      (($ float-term f) (string->number f))
      (($ function-term p b) (if (null? p) (compile-term b) `(lambda (,(string->symbol (car p))) ,(compile-term (make-function-term (cdr p) b)))))
      (($ haskell-term type term) `(,(haskell->scheme type) ,(compile-term term)))
      (($ haskell-guard-term type term) `(contract ,(haskell-contract type) ,(compile-term term) 'haskell 'scheme))
      (($ identifier-term i) `(force ,(string->symbol i)))
      (($ if-term g t e) `(if ,(compile-term g) ,(compile-term t) ,(compile-term e)))
      (($ integer-term i) (string->number i))
      (($ let-term d e) (compile-let-term d e))
      (($ list-term e) (if (null? e) null `(cons-immutable (delay ,(compile-term (car e))) (delay ,(compile-term (make-list-term (cdr e)))))))
      (($ scheme-term type identifier) `(,(scheme->haskell type) ,(string->symbol identifier)))
      (($ scheme-guard-term type term) `(contract ,(scheme-contract type) ,(compile-term term) 'scheme 'haskell))
      (($ tuple-term e) (compile-term (make-application-term (make-tuplecon-term (length e)) e)))
      (($ tuplecon-term a) (compile-tuplecon-term a))))
  
  ; function-converter :: [datum] -> datum -> datum
  (define (function-converter argument-converters result-converter)
    ; identifier :: integer -> symbol
    (define (identifier n)
      (string->symbol (string-append "x" (number->string n))))
    ; nest-parameters :: term -> integer -> integer -> datum
    (define (nest-parameters body-term parameter-number parameter-count)
      (if (< parameter-number parameter-count) body-term `(lambda (,(identifier parameter-count)) ,(nest-parameters body-term parameter-number (+ parameter-count 1)))))
    ; nest-arguments :: [datum] -> datum
    (define (nest-arguments argument-converters)
      (match argument-converters
        ((head . tail) `(,(nest-arguments tail) ,(head (identifier (length argument-converters)))))
        (() 'x)))
    `(lambda (x) ,(nest-parameters `(,result-converter ,(nest-arguments argument-converters)) (length argument-converters) 1)))
  
  (define identifier-count 0)
  
  (define (fresh-identifier)
    (set! identifier-count (+ identifier-count 1))
    (string->symbol (string-append "x" (number->string identifier-count))))
  
  ; haskell->scheme :: type -> term -> datum
  (define (haskell->scheme type term)
    (let ((id `(lambda (x) x)))
      (match type
        (($ boolean-type) term)
        (($ character-type) term)
        (($ float-type) term)
        (($ function-type p r) (let ((i (fresh-identifier)))
                                 `(lambda (,i) ,(haskell->scheme r `(,term ,(scheme->haskell p i))))))
        (($ integer-type) term)
        (($ list-type _) term)
        (($ tuple-type _) term)
        (($ type-variable _) `(make-:lump ,term)))))
  
  ; scheme->haskell :: type -> term -> datum
  (define (scheme->haskell type term)
    (let ((id `(lambda (x) x)))
      (match type
        (($ boolean-type) term)
        (($ character-type) term)
        (($ float-type) term)
        (($ function-type p r) (let ((i (fresh-identifier)))
                                 `(lambda (,i) ,(scheme->haskell r `(,term ,(haskell->scheme p i))))))
        (($ integer-type) term)
        (($ list-type type) `(foldr (lambda (x y) (cons (delay ,(scheme->haskell type `x)) (delay y))) null ,term))
        (($ tuple-type types) (let* ((pairs (zip types (list-tabulate (length types) (lambda (x) x))))
                                     (elements (map (match-lambda ((type index) `(delay (,(scheme->haskell type) (vector-ref ,term ,index))))) pairs)))
                                `(vector-immutable ,@elements)))
        (($ type-variable _) `(if (:lump? ,term) (:lump-value ,term) ,term)))))
  
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
  
  ; scheme-contract :: type -> contract
  (define (scheme-contract type)
    (match type
        (($ boolean-type) `(flat-contract boolean?))
        (($ character-type) `(flat-contract char?))
        (($ float-type) `(flat-contract number?))
        (($ function-type p r) `(-> ,(haskell-contract p) ,(scheme-contract r)))
        (($ integer-type) `(flat-contract integer?))
        (($ list-type type) `(and/c (list-immutableof ,(haskell-contract type))
                                    (flat-contract proper-list?)
                                    (flat-contract (lambda (x) (not (circular-list? x))))))
        (($ tuple-type types) `(vector-immutable/c ,@(map scheme-contract types)))
        (($ type-constructor identifier) (scheme-contract (translate-type-constructor (make-type-constructor identifier))))
        (($ type-variable _) `any/c)))
  
  ; compile-let-term :: [declaration-term] -> term -> datum
  (define (compile-let-term d e)
    (define compile-declaration-term
      (match-lambda (($ declaration-term p e) `(,(string->symbol (car p)) (delay ,(if (null? (cdr p))
                                                                                      (compile-term e)
                                                                                      (compile-term (make-function-term (cdr p) e))))))))
    `(letrec ,(map compile-declaration-term d) ,(compile-term e)))
  
  ; compile-application-term :: term -> [term] -> datum
  (define (compile-application-term f a)
    (if (null? a) (compile-term f) `(,(compile-application-term f (cdr a)) (delay ,(compile-term (car a))))))
  
  ; compile-tuplecon-term :: integer -> datum
  (define (compile-tuplecon-term a)
    (define (enumerate n)
      (if (= n 0) null (cons n (enumerate (- n 1)))))
    (define (nest n)
      (if (= n (+ a 1))
          `(vector-immutable ,@(map (lambda (x) (string->symbol (string-append "x" (number->string x)))) (reverse (enumerate a))))
          `(lambda (,(string->symbol (string-append "x" (number->string n)))) ,(nest (+ n 1)))))
    (nest 1))
  
  ; characters :: immutable-hash-table
  (define characters
    (make-immutable-hash-table `() 'equal))
  
  ; tests :: [test]
  #;(define tests
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
  
  #;(define (run-all-tests)
    (run-tests (lambda (x) (compile-term x)) tests)))