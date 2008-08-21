(module types mzscheme
  (provide (all-defined))
  
  (define-struct type () #f)
  
  (define-struct (forall-type type) (variable type) #f)
  
  (define-struct (function-constructor type) () #f)
  
  (define-struct (list-constructor type) () #f)
  
  (define-struct (tuple-constructor type) (arity) #f)
  
  (define-struct (type-application type) (operator operand) #f)
  
  (define-struct (type-constructor type) (identifier) #f)
  
  (define-struct (type-variable type) (identifier) #f)
  
  (define-struct (unit-type type) () #f))