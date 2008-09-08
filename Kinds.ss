(module Kinds mzscheme
  (provide (all-defined))
  
  (define-struct Kind () #f)
  
  (define-struct (Nullary Kind) () #f)
  
  (define-struct (Binary Kind) () #f))