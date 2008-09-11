(module Converters mzscheme
  (require (only (lib "1.ss" "srfi") list-tabulate zip)
           (only (lib "match.ss") match match-lambda)
           (lib "HaskellSyntax.ss" "sham" "haskell"))
  
  (provide convertHM #;convertHS convertMH #;convertSH)
  
  ; convertHM :: HaskellSyntax datum -> datum
  (define (convertHM type syntax)
    (match type
      (($ FunctionType p r) (let ((n (newName))) `(lambda (,n) ,(convertHM r `(,syntax ,(convertMH p `(force ,n)))))))
      (($ ListType t) `(foldr (lambda (x y) (cons (delay ,(convertHM t 'x)) (delay y))) null ,syntax))
      (($ TupleType ts) `((lambda (x) (vector-immutable ,@(map (match-lambda ((t i) `(delay ,(convertHM t `(vector-ref x ,i)))))
                                                                 (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      (($ TypeConstructor "Bool") `(if ,syntax (force haskell:True) (force haskell:False)))
      (($ TypeConstructor "Char") (error 'convertHM "ML does not have characters"))
      (($ TypeConstructor "String") `(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list ,syntax)))
      (($ TypeConstructor _) syntax)
      (($ TypeVariable _) syntax)
      (($ UnitType) syntax)))
  
  ; convertHS :: HaskellSyntax datum -> datum
  #;(define (convertHS type syntax)
      (let ((forced `((lambda (x) (if (promise? x) (force x) x)) ,syntax)))
        (match type
          (($ FunctionType p r) (let ((n (newName))) `(lambda (,n) ,(convertHS r `(,forced ,(convertSH p n))))))
          (($ ListType t) `(foldr (lambda (x y) (cons (delay ,(convertHM t 'x)) (delay y))) null ,forced))
          (($ TupleType t) `(lambda (x) (vector-immutable ,@(map (match-lambda ((t i) `(delay ,(convertHM t `(vector-ref x ,i)))))
                                                                   (zip ts (list-tabulate (length ts) (lambda (x) x)))))))
          (($ TypeConstructor "Bool") forced)
          (($ TypeConstructor "Char") forced)
          (($ TypeConstructor "Float") forced)
          (($ TypeConstructor "Int") forced)
          (($ TypeConstructor "String") `(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list ,syntax)))
          (($ TypeConstructor n) !!!TODO)
          (($ type-variable _) `(let ((x ,f)) (if (lump? x) (force (lump-contents x)) x))))))
  
  ; convertMH :: HaskellSyntax datum -> datum
  (define (convertMH type syntax)
    (match type
      (($ FunctionType p r) (let ((n (newName))) `(lambda (,n) ,(convertMH r `(,syntax (delay ,(convertHM p n)))))))
      (($ ListType t) syntax)
      (($ TupleType ts) `((lambda (x) (vector-immutable ,@(map (match-lambda ((t i) `(delay ,(convertHM t `(vector-ref x ,i)))))
                                                                 (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      (($ TypeConstructor "Bool") `(if (haskell:True? ,syntax) #t #f))
      (($ TypeConstructor "Char") (error 'convertMH "ML does not have characters"))
      (($ TypeConstructor _) syntax)
      (($ TypeVariable _) syntax)
      (($ UnitType) syntax)))
  
  ; convertSH :: Type CoreSyntax datum -> datum
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
  
  ; newName :: datum
  (define (newName)
    (set! counter (+ counter 1))
    (string->symbol (string-append "x" (number->string counter)))))