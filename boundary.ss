(module boundary mzscheme
  (require (lib "compiler.ss" "haskell")
           (lib "contract.ss")
           (lib "list.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "types.ss" "haskell"))
  
  (provide scheme-haskell-boundary)

  ; translate-type :: type -> type
  (define (translate-type type)
    (match type
      (($ function-type types) (make-function-type (map translate-type types)))
      (($ list-type type) (make-list-type (translate-type type)))
      (($ tuple-type types) (make-tuple-type (map translate-type types)))
      (($ type-constructor "Bool") (make-boolean-type))
      (($ type-constructor "Char") (make-character-type))
      (($ type-constructor "Int") (make-integer-type))
      (($ type-constructor "Integer") (make-integer-type))
      (($ type-constructor "Float") (make-float-type))
      (t t)))
  
  ; type->contract :: type -> contract
  (define (type->contract type)
    (match type
      (($ boolean-type) (flat-contract boolean?))
      (($ character-type) (flat-contract char?))
      (($ float-type) (flat-contract number?))
      (($ function-type types) (foldr1 (lambda (x y) (-> (type->contract x) (type->contract y))) types))
      (($ integer-type) (flat-contract integer?))
      (($ list-type type) (cons-immutable/c (type->contract type) pair?))
      (($ tuple-type types) (vector-immutable/c (map type->contract types)))
      (($ type-constructor identifier) (type->contract (translate-type (make-type-constructor identifier))))
      (($ type-variable _) any/c)
      (($ universal-type type) (type->contract type))))
  
  (define (thunk-arguments function parameter-number)
    (define (nest-arguments function argument-count)
      (if (equal? argument-count parameter-number)
    (define (nest-functions function nest-count)
      (if (equal? nest-count parameter-number)
          function
          '(lambda (,(string->symbol (string-append "p" (+ nest-count 1)))) 
    (if (equal? parameter-number 0)
        function
        '(lambda (,(string->symbol ))
    
  
  ; scheme-haskell-boundary :: term -> type -> term
  (define (scheme-haskell-boundary term type)
    (if (function-type? type)
      
      (contract (type->contract type) (compile-term term) 'haskell 'scheme))))
