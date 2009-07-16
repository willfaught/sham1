(module Primitives scheme
  (provide (all-defined-out))
  
  (define-struct constructor/Char# (value) #:transparent)
  
  (define-struct constructor/Float# (value) #:transparent)
  
  (define-struct constructor/Int# (value) #:transparent)
  
  (define-contract-struct constructor/Tuple# (values))
  
  (define (coffer)
    (define-struct coffer (value) #:transparent)
    (list coffer? make-coffer coffer-value)))