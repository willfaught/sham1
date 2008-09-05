(module types mzscheme
  (provide (all-defined))
  
  (define-struct type () #f)
  
  (define-struct (application type) (operator operand) #f)
  
  (define-struct (constructor type) (name) #f)
  
  (define-struct (function type) () #f)
  
  (define-struct (list type) () #f)
  
  (define-struct (tuple type) (arity) #f)
  
  (define-struct (variable type) (name) #f)
  
  (define-struct (unit type) () #f))