(module boundary mzscheme
  (require (lib "contract.ss")
           (lib "list.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "types.ss" "haskell"))
  
  (provide scheme-haskell-boundary)
  
  ; type->contract :: type -> contract
  (define (type->contract type)
    (match type
      (($ boolean-type) (flat-contract boolean?))
      (($ character-type) (flat-contract char?))
      (($ float-type) (flat-contract number?))
      (($ function-type types) (foldr1 (lambda (x y) (-> (type->contract x) (type->contract y))) types))
      (($ integer-type) (flat-contract integer?))
      (($ list-type type) (cons-immutable (type->contract type) pair?))
      (($ tuple-type types) (vector-immutable/c (map type->contract types)))
      (($ type-constructor identifier) )
      (($ type-variable identifier) )
      (($ universal-type type) )))
  
  ; scheme-haskell-boundary :: term -> type -> term
  (define (scheme-haskell-boundary term type)
    (match type
      (($ boolean-type) (
  )
