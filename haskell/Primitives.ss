(module Primitives scheme
  (provide (all-defined-out))
  
  (define-struct Char# () #:transparent)
  
  (define-struct Float# () #:transparent)
  
  (define-struct Int# () #:transparent)
  
  (define-struct Tuple# () #:transparent)
  
  (define (coffer)
    (define-struct coffer (value) #:transparent)
    (list make-coffer coffer-value coffer?)))