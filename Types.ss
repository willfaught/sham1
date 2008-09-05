(module Types mzscheme
  (provide (all-defined))
  
  (define-struct Type () #f)
  
  (define-struct (Application Type) (operator operand) #f)
  
  (define-struct (Constructor Type) (name) #f)
  
  (define-struct (Function Type) () #f)
  
  (define-struct (List Type) () #f)
  
  (define-struct (Tuple Type) (arity) #f)
  
  (define-struct (Variable Type) (name) #f)
  
  (define-struct (Unit Type) () #f))