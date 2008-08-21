(module types mzscheme
  (provide (all-defined))
  
  (define-struct type () #f)
  
  (define-struct (forall-type type) (variable type) #f)
  
  (define-struct (type-application type) (operator operand) #f)
  
  (define-struct (type-constructor type) (identifier) #f)
  
  (define-struct (type-variable type) (identifier) #f))