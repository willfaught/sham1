(module Primitives scheme
  (provide Char/haskell/c
           Float/haskell/c
           Int/haskell/c
           Tuple#/haskell/c
           (rename-out (constructor/Char-value variable/charValue#)
                       (constructor/Float-value variable/floatValue#)
                       (constructor/Int-value variable/intValue#)
                       (make-constructor/Char variable/Char)
                       (make-constructor/Float variable/Float)
                       (make-constructor/Int variable/Int)
                       (make-constructor/Tuple# variable/Tuple#)))
                       
  (define-struct constructor/Char (value) #:transparent)
  
  (define-struct constructor/Float (value) #:transparent)
  
  (define-struct constructor/Int (value) #:transparent)
  
  (define-struct constructor/Tuple# (values) #:transparent)
  
  (define Char/haskell/c
    (struct/c constructor/Char char?))
  
  (define Float/haskell/c
    (struct/c constructor/Float number?))
  
  (define Int/haskell/c
    (struct/c constructor/Int integer?))
  
  (define (Tuple#/haskell/c c)
    (struct/c constructor/Char c))
  
  (define Function#?
    (lambda (contract1)
      (lambda (contract2)
        (-> contract1 contract2)))))