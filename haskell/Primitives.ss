(module Primitives scheme
  (provide (all-defined-out))
  
  (define-struct Char# (value) #:transparent)
  
  (define-struct Float# (value) #:transparent)
  
  (define-struct Int# (value) #:transparent)
  
  (define-struct Tuple# (values) #:transparent)
  
  (define (coffer)
    (define-struct coffer (value) #:transparent)
    (list make-coffer coffer-value coffer?)))