(module Primitives scheme
  (provide (all-defined-out))
  
  (define-struct constructor/Char# (value) #:transparent)
  
  (define-struct constructor/Float# (value) #:transparent)
  
  (define-struct constructor/Int# (value) #:transparent)
  
  (define-struct constructor/Tuple# (values) #:transparent)
  
  (define-contract-struct constructor/Cons# (head tail))
  
  (define-contract-struct constructor/False ())
  
  (define-contract-struct constructor/Nil# ())
  
  (define-contract-struct constructor/True ())
  
  (define-contract-struct constructor/Unit# ())
  
  (define (coffer)
    (define-struct coffer (value) #:transparent)
    (list make-coffer coffer-value coffer?)))