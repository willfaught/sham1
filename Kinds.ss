(module Kinds scheme
  (provide (all-defined-out))
  
  (define-struct Kind () #:transparent)
  
  (define-struct (Nullary Kind) () #:transparent)
  
  (define-struct (Binary Kind) () #:transparent))
