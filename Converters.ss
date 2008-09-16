(module Converters mzscheme
  (require (only (lib "1.ss" "srfi") list-tabulate zip)
           (only (lib "match.ss") match match-lambda)
           (lib "HaskellSyntax.ss" "sham" "haskell"))
  
  (provide convertHM convertHS convertMH convertSH)
  
  (define-struct Wrapper (name value) #f)
  
  ; convertHM :: HaskellSyntax datum -> datum
  (define (convertHM type syntax)
    (convertHM2 type syntax 1))
  
  ; convertHM2 :: HaskellSyntax datum integer -> datum
  (define (convertHM2 type syntax depth)
    (match type
      (($ FunctionType p r) (let ((n (name depth))) `(lambda (,n) ,(convertHM2 r `(,syntax ,(convertMH2 p `(force ,n) (+ depth 1))) (+ depth 1)))))
      (($ ListType t) `(foldr (lambda (x y) (cons (delay ,(convertHM2 t 'x depth)) (delay y))) null ,syntax))
      (($ TupleType ts) `((lambda (x) (vector-immutable ,@(map (match-lambda ((t i) `(delay ,(convertHM2 t `(vector-ref x ,i) depth))))
                                                               (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      (($ TypeConstructor "Bool") `(if ,syntax (force haskell:True) (force haskell:False)))
      (($ TypeConstructor "Char") (error 'convertHM "ML does not have characters"))
      (($ TypeConstructor "String") `(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list ,syntax)))
      (($ TypeConstructor _) syntax)
      (($ TypeVariable _) syntax)
      (($ UnitType) syntax)))
  
  ; convertHS :: HaskellSyntax datum -> datum
  (define (convertHS type syntax)
    (convertHS type syntax 1))
  
  ; convertHS2 :: HaskellSyntax datum integer -> datum
  (define (convertHS2 type syntax depth)
    (match type
      (($ FunctionType p r) (let ((n (name depth))) `(lambda (,n) ,(convertHS2 r `(,syntax ,(convertSH2 p `(force ,n) (+ depth 1))) (+ depth 1)))))
      (($ ListType t) `(foldr (lambda (x y) (cons (delay ,(convertHS2 t 'x)) (delay y))) null ,syntax))
      (($ TupleType ts) `((lambda (x) (vector-immutable ,@(map (match-lambda ((t i) `(delay ,(convertHS2 t `(vector-ref x ,i) depth))))
                                                               (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      (($ TypeConstructor "Bool") `(if ,syntax (force haskell:True) (force haskell:False)))
      (($ TypeConstructor "Char") syntax)
      (($ TypeConstructor "String") `(foldr (lambda (x y) (cons (delay x) (delay y))) null (string->list ,syntax)))
      (($ TypeConstructor _) syntax)
      (($ TypeVariable n) `((lambda (x) (if (and (Wrapper? x) (equal? (Wrapper-name x) ,n)) (Wrapper-value x) (error (format "Scheme violated parametricity")))) ,syntax))
      (($ UnitType) syntax)))
  
  ; convertMH :: HaskellSyntax datum -> datum
  (define (convertMH type syntax)
    (convertMH2 type syntax 1))
  
  ; convertMH2 :: HaskellSyntax datum integer -> datum
  (define (convertMH2 type syntax depth)
    (match type
      (($ FunctionType p r) (let ((n (name depth))) `(lambda (,n) ,(convertMH2 r `(,syntax (delay ,(convertHM2 p n (+ depth 1)))) (+ depth 1)))))
      (($ ListType _) syntax)
      (($ TupleType ts) `((lambda (x) (vector-immutable ,@(map (match-lambda ((t i) (convertMH2 t `(vector-ref x ,i) (+ depth 1))))
                                                               (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      (($ TypeConstructor "Bool") `(if (haskell:True? ,syntax) #t #f))
      (($ TypeConstructor "Char") (error 'convertMH2 "ML does not have characters"))
      (($ TypeConstructor _) syntax)
      (($ TypeVariable _) syntax)
      (($ UnitType) syntax)))
  
  ; convertSH :: HaskellSyntax datum -> datum
  (define (convertSH type syntax)
    (convertSH2 type syntax 1))
  
  ; convertSH2 :: HaskellSyntax datum integer -> datum
  (define (convertSH2 type syntax depth)
    (match type
      (($ FunctionType p r) (let ((n (name depth))) `(lambda (,n) ,(convertSH2 r `(,syntax (delay ,(convertHS2 p n (+ depth 1)))) (+ depth 1)))))
      (($ ListType _) syntax)
      (($ TupleType ts) `((lambda (x) (vector-immutable ,@(map (match-lambda ((t i) (convertSH2 t `(vector-ref x ,i) (+ depth 1))))
                                                               (zip ts (list-tabulate (length ts) (lambda (x) x)))))) ,syntax))
      (($ TypeConstructor "Bool") `(if (haskell:True? ,syntax) #t #f))
      (($ TypeConstructor _) syntax)
      (($ TypeVariable n) `(make-Wrapper ,n ,syntax))
      (($ UnitType) syntax)))
  
  ; name :: integer -> datum
  (define (name depth)
    (string->symbol (string-append "x" (number->string depth)))))