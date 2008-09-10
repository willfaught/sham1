(module Boundaries mzscheme
  (require (only (lib "1.ss" "srfi") list-tabulate zip)
           (only (lib "match.ss") match match-lambda)
           (prefix c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (prefix t/ (lib "Types.ss" "sham")))
  
  (provide convertHM #;convertHS convertMH #;convertSH)
  
  ; convertHM :: h/HaskellSyntax c/CoreSyntax -> datum
  (define (convertHM type syntax)
    (let ((forced `((lambda (x) (if (promise? x) (force x) x)) ,syntax)))
      (match type
        (($ h/FunctionType p r) (let ((n (newName))) `(lambda (,n) ,(convertHM r `(,forced ,(convertMH p n))))))
        (($ h/ListType t) `(foldr (lambda (x y) (cons (delay ,(convertHM t 'x)) (delay y))) null ,forced))
        (($ h/TupleType ts) `((lambda (x) (vector-immutable ,@(map (match-lambda ((t i) `(delay ,(convertHM t `(vector-ref x ,i)))))
                                                                   (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,forced))
        (($ h/TypeConstructor "Bool") `(if ,forced (force haskell:True) (force haskell:False)))
        (($ h/TypeConstructor "String") `(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list ,forced)))
        (($ h/TypeConstructor _) forced)
        (($ h/TypeVariable _) forced)
        (($ h/UnitType) forced))))
  
  ; convertHS :: h/HaskellSyntax c/CoreSyntax -> datum
  #;(define (convertHS type syntax)
      (let ((forced `((lambda (x) (if (promise? x) (force x) x)) ,syntax)))
        (match type
          (($ h/FunctionType p r) (let ((n (newName))) `(lambda (,n) ,(convertHS r `(,forced ,(convertSH p n))))))
          (($ h/ListType t) `(foldr (lambda (x y) (cons (delay ,(convertHM t 'x)) (delay y))) null ,forced))
          (($ h/TupleType t) `(lambda (x) (vector-immutable ,@(map (match-lambda ((t i) `(delay ,(convertHM t `(vector-ref x ,i)))))
                                                                   (zip ts (list-tabulate (length ts) (lambda (x) x)))))))
          (($ h/TypeConstructor "Bool") forced)
          (($ h/TypeConstructor "Char") forced)
          (($ h/TypeConstructor "Float") forced)
          (($ h/TypeConstructor "Int") forced)
          (($ h/TypeConstructor "String") `(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list ,syntax)))
          (($ h/TypeConstructor n) !!!TODO)
          (($ type-variable _) `(let ((x ,f)) (if (lump? x) (force (lump-contents x)) x))))))
  
  ; convertMH :: h/HaskellSyntax c/CoreSyntax -> datum
  (define (convertMH type syntax)
    (match type
      (($ h/FunctionType p r) (let ((n (newName))) `(lambda (,n) ,(convertMH r `(,syntax (delay ,(convertHM p n)))))))
      (($ h/ListType t) syntax)
      (($ h/TupleType ts) `((lambda (x) (vector-immutable ,@(map (match-lambda ((t i) `(delay ,(convertHM t `(vector-ref x ,i)))))
                                                                 (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      (($ h/TypeConstructor "Bool") `(if (haskell:True? ,syntax) #t #f))
      (($ h/TypeConstructor "Char") (error 'convertMH "ML does not have characters"))
      (($ h/TypeConstructor _) syntax)
      (($ h/TypeVariable n) syntax)
      (($ h/UnitType) syntax)))
  
  ; convertSH :: Type CoreSyntax integer -> datum
  #;(define (convertSH type term depth)
      (let ((id `(lambda (x) x)))
        (match type
          (($ character-type) term)
          (($ float-type) term)
          (($ function-type p r) (let ((i (identifier depth)))
                                   `(lambda (,i) ,(convertSH r `(,term (delay ,(convertHS p i (+ depth 1)))) (+ depth 1)))))
          (($ integer-type) term)
          (($ list-type _) term)
          (($ tuple-type _) term)
          (($ type-constructor _) term)
          (($ type-variable _) `(make-lump ,term)))))
  
  ; counter :: integer
  (define counter 0)
  
  ; newName :: symbol
  (define (newName)
    (set! counter (+ counter 1))
    (string->symbol (string-append "x" (number->string counter)))))